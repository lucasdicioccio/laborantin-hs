{-# LANGUAGE OverloadedStrings #-}

module Labor where
 
{- TODO 
 -
 - logging
 - state passing between phases (variables)
 - execution engine
 - resource locking
 - distributed process backend 
 -
 -}

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import Control.Applicative ((<$>),(<*>))
import Control.Monad.State
import Control.Monad.Reader
import Data.Aeson (decode,encode,FromJSON,parseJSON,(.:),ToJSON,toJSON,(.=),object)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.List
import Data.UUID
import System.Directory
import System.Random

type ParameterSpace = M.Map String ParameterDescription
type ParameterGenerator = ParameterDescription -> [ParameterValue]
type Step m a = ReaderT (Backend m,Execution) m a
newtype Action = Action { unAction :: Step IO ()}

instance Show Action where
  show _ = "(Action)"

data ScenarioDescription = SDesc {
    sName   :: String
  , sDesc   :: String
  , sParams :: ParameterSpace
  , sHooks  :: M.Map String Action
  } deriving (Show)

data ParameterDescription = PDesc {
    pName   :: String
  , pDesc   :: String
  , pValues :: [ParameterValue]
  } deriving (Show)

data ParameterValue = StringParam String 
  | NumberParam Rational
  | Array [ParameterValue]
  deriving (Show,Eq,Ord)

type ParameterSet = M.Map String ParameterValue

data ExecutionStatus = Prepared | Running | Success | Failure | Loaded
  deriving (Show,Read)

data Execution = Exec {
    eScenario :: ScenarioDescription
  , eParamSet :: ParameterSet
  , ePath     :: String
  , eStatus   :: ExecutionStatus
} deriving (Show)

data StoredExecution = Stored {
    seParamSet :: ParameterSet
  , sePath     :: String
  , seStatus   :: ExecutionStatus
} deriving (Show)

instance ToJSON ParameterValue where
    toJSON (StringParam str) = object ["type" .= ("string"::T.Text), "val" .= T.pack str]
    toJSON (NumberParam n)   = object ["type" .= ("num"::T.Text), "val" .= n]
    toJSON (Array xs)        = toJSON xs

instance ToJSON ExecutionStatus where
    toJSON = toJSON . show

instance ToJSON Execution where
    toJSON (Exec sc params path status) = object    [ "scenario-name" .= sName sc
                                                    , "params" .= params
                                                    , "path" .= path
                                                    , "status" .= status
                                                    ] 

instance FromJSON ParameterValue where
    parseJSON (A.Object v) = (v .: "type") >>= match
        where match :: T.Text -> A.Parser ParameterValue
              match "string" = StringParam <$> v .: "val"
              match "num"    = NumberParam <$> v .: "val"
              match _        = mzero
    
    parseJSON _ = mzero

instance FromJSON ExecutionStatus where
    parseJSON (A.String txt) = return $ read $ T.unpack txt
    parseJSON _ = mzero

instance FromJSON StoredExecution where
    parseJSON (A.Object v) = Stored <$>
                               v .: "params" <*>
                               v .: "path" <*>
                               v .: "status"
    parseJSON _          = mzero

data Backend m = Backend {
    bName      :: String
  , bPrepareExecution :: ScenarioDescription -> ParameterSet -> m Execution
  , bAdvertise :: Execution -> Step m ()
  , bSetup     :: Execution -> Step m ()
  , bRun       :: Execution -> Step m ()
  , bTeardown  :: Execution -> Step m ()
  , bAnalyze   :: Execution -> Step m ()
  , bResult    :: Execution -> String -> Step m (Result m)
  , bLoad      :: ScenarioDescription -> m [Execution]
}

data Result m = Result {
    pPath   :: String
  , pRead   :: Step m (String)
  , pAppend :: String -> Step m ()
  , pWrite  :: String -> Step m ()
}

defaultBackend :: Backend IO
defaultBackend = Backend "default IO backend" prepare advertise setup run teardown analyze result load
  where prepare sc params = do
                  uuid <- liftIO $ (randomIO :: IO (UUID))
                  let rundir = intercalate "/" [".", sName sc, show uuid]
                  let exec = Exec sc params rundir Prepared
                  liftIO $ createDirectoryIfMissing True rundir
                  liftIO $ BSL.writeFile (rundir ++ "/execution.json") (encode exec)
                  return exec
        advertise exec    = do
                  let lines = unlines $ map ($ exec) [show. sName . eScenario, show . eParamSet, ePath]
                  liftIO $ putStrLn lines
        setup             = callHooks "setup" . eScenario
        run               = callHooks "run" . eScenario
        teardown          = callHooks "teardown" . eScenario
        analyze           = callHooks "analyze" . eScenario
        callHooks key sc  = maybe (error $ "no such hook: " ++ key) unAction (M.lookup key $ sHooks sc)

        result exec       = return . defaultResult exec

        load sc           = do
            dirs <- filter notDot <$> getDirectoryContents (sName sc)
            let paths = map ((sName sc ++ "/") ++) dirs
            mapM loadOne paths
            where notDot dirname = not (take 1 dirname == ".")
                  loadOne path = do
                    exec' <- (liftM forStored) . decode <$> BSL.readFile (path ++ "/execution.json")
                    maybe (error $ "decoding: " ++ path) return exec'

                    where forStored (Stored params path status) = Exec sc params path status

defaultResult :: Execution -> String -> Result IO
defaultResult exec name = Result path read append write
  where read        = liftIO $ readFile path
        append dat  = liftIO $ appendFile path dat
        write dat   = liftIO $ writeFile path dat
        path        = intercalate "/" [ePath exec, name]

execute :: MonadIO m => Backend m -> ScenarioDescription -> ParameterSet -> m ()
execute b sc prm = do
  exec <- bPrepareExecution b sc prm 
  runReaderT (go exec) (b, exec)
  where go exec = do 
              bAdvertise b $ exec
              bSetup b $ exec
              bRun b $ exec
              bTeardown b $ exec
              bAnalyze b $ exec

load :: MonadIO m => Backend m -> ScenarioDescription -> m [Execution]
load b = bLoad b

{- 
 - DSL
 -}

class Describable a where
  changeDescription :: String -> a -> a

instance Describable ScenarioDescription where
  changeDescription d sc = sc { sDesc = d }

instance Describable ParameterDescription where
  changeDescription d pa = pa { pDesc = d }

scenario :: String -> (State ScenarioDescription ()) -> ScenarioDescription
scenario name f = snd $ runState f sc0
  where sc0 = SDesc name "" M.empty M.empty

describe :: Describable a => String -> State a ()
describe desc = modify (changeDescription desc)

parameter :: String -> (State ParameterDescription ()) -> State ScenarioDescription ()
parameter name f = modify (addParam name param)
  where addParam k v sc0 = sc0 { sParams = M.insert k v (sParams sc0) }
        param = snd $ runState f param0
                where param0 = PDesc name "" []

values :: [ParameterValue] -> State ParameterDescription ()
values xs = do
  param0 <- get
  put $ param0 { pValues = xs }

str :: String -> ParameterValue
str = StringParam

num :: Integer -> ParameterValue
num = NumberParam . fromInteger

arr :: [ParameterValue] -> ParameterValue
arr = Array

setup :: Step IO () -> State ScenarioDescription ()
setup = appendHook "setup"

run :: Step IO () -> State ScenarioDescription ()
run = appendHook "run"

teardown :: Step IO () -> State ScenarioDescription ()
teardown  = appendHook "teardown"

analyze :: Step IO () -> State ScenarioDescription ()
analyze = appendHook "analyze"

appendHook :: String -> Step IO () -> State ScenarioDescription ()
appendHook name f = modify (addHook name $ Action f)
  where addHook k v sc0 = sc0 { sHooks = M.insert k v (sHooks sc0) }

result :: Monad m => String -> Step m (Result m)
result name = do 
  (b,r) <- ask
  (bResult b) r name

writeResult :: Monad m => String -> String -> Step m ()
writeResult name dat = do
  result name >>= (flip pWrite) dat

appendResult :: Monad m => String -> String -> Step m ()
appendResult name dat = do
  result name >>= (flip pAppend) dat

{- 
 - Example
 -}

vals :: ParameterSet
vals = M.fromList [("destination",str "probecraft.net"),("packet size",num 32)]

ping :: ScenarioDescription
ping = scenario "ping" $ do
  describe "ping to a remote server"
  parameter "destination" $ do
    describe "a destination server (host or ip)"
    values $ [str "example.com", str "probecraft.net"]
  parameter "packet size" $ do
    describe "packet size in bytes"
    values $ [num 50, num 1500]
  setup $ do
      writeResult "foobar" "bla"
      lift $ print "setup action"
  teardown $ (lift $ print "teardown action")
  run $ (lift $ print "run action")
  analyze $ (lift $ print "analyze action")
