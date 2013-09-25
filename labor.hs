{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Labor where
 
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import Control.Applicative ((<$>),(<*>))
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Exception
import Data.Aeson (decode,encode,FromJSON,parseJSON,(.:),ToJSON,toJSON,(.=),object)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.List
import Data.UUID
import System.Directory
import System.Random
import System.Log.Logger
import System.Log.Handler (close)
import System.Log.Handler.Simple
import System.Log.Handler.Log4jXML
import Data.Dynamic

type ParameterSpace = M.Map String ParameterDescription
type ParameterGenerator = ParameterDescription -> [ParameterValue]
data ExecutionError = ExecutionError String
    deriving (Show)
instance Error ExecutionError where
  noMsg    = ExecutionError "A String Error!"
  strMsg   = ExecutionError
type Step m a = ErrorT ExecutionError (ReaderT (Backend m,Execution m) m) a
type DynEnv = M.Map String Dynamic
type EnvIO = (StateT DynEnv IO)

runEnvIO :: EnvIO () -> IO ((), DynEnv)
runEnvIO m = runStateT m M.empty

newtype Action m = Action { unAction :: Step m () }

instance Show (Action m) where
  show _ = "(Action)"

data ScenarioDescription m = SDesc {
    sName   :: String
  , sDesc   :: String
  , sParams :: ParameterSpace
  , sHooks  :: M.Map String (Action m)
  } deriving (Show)

data ParameterDescription = PDesc {
    pName   :: String
  , pDesc   :: String
  , pValues :: [ParameterValue]
  } deriving (Show,Eq,Ord)

data ParameterValue = StringParam String 
  | NumberParam Rational
  | Array [ParameterValue]
  deriving (Show,Eq,Ord)

type ParameterSet = M.Map String ParameterValue

data ExecutionStatus = Running | Success | Failure 
  deriving (Show,Read)

data Execution m = Exec {
    eScenario :: ScenarioDescription m
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

instance ToJSON (Execution a) where
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

type Finalizer m = Execution m -> m ()

data Backend m = Backend {
    bName      :: String
  , bPrepareExecution  :: ScenarioDescription m -> ParameterSet -> m (Execution m,Finalizer m)
  , bFinalizeExecution :: Execution m -> Finalizer m -> m ()
  , bSetup     :: Execution m -> Step m ()
  , bRun       :: Execution m -> Step m ()
  , bTeardown  :: Execution m -> Step m ()
  , bAnalyze   :: Execution m -> Step m ()
  , bRecover   :: Execution m -> Step m ()
  , bResult    :: Execution m -> String -> Step m (Result m)
  , bLoad      :: ScenarioDescription m -> m [Execution m]
  , bLogger    :: Execution m -> Step m (LogHandler m)
}

data Result m = Result {
    pPath   :: String
  , pRead   :: Step m String
  , pAppend :: String -> Step m ()
  , pWrite  :: String -> Step m ()
}

newtype LogHandler m = LogHandler { lLog :: String -> Step m () }

loggerName :: Execution m -> String
loggerName exec = "laborantin:" ++ ePath exec

defaultBackend :: Backend EnvIO
defaultBackend = Backend "default EnvIO backend" prepare finalize setup run teardown analyze recover result load log
  where prepare :: ScenarioDescription EnvIO -> ParameterSet -> EnvIO (Execution EnvIO,Finalizer EnvIO)
        prepare sc params = do
                  uuid <- liftIO (randomIO :: IO UUID)
                  let rundir = intercalate "/" [sName sc, show uuid]
                  let exec = Exec sc params rundir Running
                  handles <- liftIO $ do
                    createDirectoryIfMissing True rundir
                    BSL.writeFile (rundir ++ "/execution.json") (encode exec)
                    updateGlobalLogger (loggerName exec) (setLevel DEBUG)
                    h1 <- fileHandler (rundir ++ "/execution-log.txt") DEBUG
                    h2 <- log4jFileHandler (rundir ++ "/execution-log.xml") DEBUG
                    forM_ [h1,h2] (updateGlobalLogger (loggerName exec) . addHandler)
                    putStrLn $ advertise exec
                    return [h1,h2]
                  return (exec, \_ -> liftIO $ forM_ handles close)
                  where advertise exec = unlines $ map ($ exec) [show. sName . eScenario
                                                , show . eParamSet
                                                , ePath]
        finalize  exec finalizer = do
                            finalizer exec
                            liftIO . putStrLn $ "done"
                            liftIO $ BSL.writeFile (rundir ++ "/execution.json") (encode exec)
                            where rundir = ePath exec
        setup             = callHooks "setup" . eScenario
        run               = callHooks "run" . eScenario
        teardown          = callHooks "teardown" . eScenario
        analyze           = callHooks "analyze" . eScenario
        recover           = callHooks "recover" . eScenario
        callHooks key sc  = maybe (error $ "no such hook: " ++ key) unAction (M.lookup key $ sHooks sc)
        result exec       = return . defaultResult exec

        load :: ScenarioDescription EnvIO -> EnvIO [Execution EnvIO]
        load sc           = liftIO $ do
            dirs <- filter notDot <$> getDirectoryContents (sName sc)
            let paths = map ((sName sc ++ "/") ++) dirs
            mapM loadOne paths
            where notDot dirname = take 1 dirname /= "."
                  loadOne path = do
                    exec' <- liftM forStored . decode <$> BSL.readFile (path ++ "/execution.json")
                    maybe (error $ "decoding: " ++ path) return exec'

                    where forStored (Stored params path status) = Exec sc params path status
        log exec          = return $ defaultLog exec

defaultResult :: Execution m -> String -> Result EnvIO
defaultResult exec name = Result path read append write
  where read        = liftIO $ readFile path
        append dat  = liftIO $ appendFile path dat
        write dat   = liftIO $ writeFile path dat
        path        = intercalate "/" [ePath exec, name]

defaultLog :: Execution m -> LogHandler EnvIO
defaultLog exec = LogHandler logF
    where logF msg = liftIO $ debugM (loggerName exec) msg
          path = ePath exec ++ "/execution.log"

execute' :: (MonadIO m) => Backend m -> ScenarioDescription m -> ParameterSet -> m ()
execute' b sc prm = execution
  where execution = do
            (exec,final) <- bPrepareExecution b sc prm 
            status <- runReaderT (runErrorT (go exec `catchError` recover exec)) (b, exec)
            let exec' = either (\_ -> exec {eStatus = Failure}) (\_ -> exec {eStatus = Success}) status
            bFinalizeExecution b exec' final
            where go exec = do 
                        bSetup b exec
                        bRun b exec
                        bTeardown b exec
                        bAnalyze b exec
                  recover exec err = bRecover b exec >> throwError err

executeExhaustive :: (MonadIO m) => Backend m -> ScenarioDescription m -> m ()
executeExhaustive b sc = mapM_ f $ paramSets $ sParams sc
    where f = execute' b sc 

executeMissing :: (MonadIO m) => Backend m -> ScenarioDescription m -> m ()
executeMissing b sc = do
    execs <- load b sc
    let exhaustive = S.fromList $ paramSets (sParams sc)
    let existing = S.fromList $ map eParamSet execs
    mapM_ f $ S.toList (exhaustive `S.difference` existing)
    where f = execute' b sc

paramSets :: ParameterSpace -> [ParameterSet]
paramSets ps = map M.fromList $ sequence possibleValues
    where possibleValues = map f $ M.toList ps
          f (k,desc) = map (pName desc,) $ pValues desc

load :: (MonadIO m) => Backend m -> ScenarioDescription m -> m [Execution m]
load = bLoad

{- 
 - DSL
 -}

class Describable a where
  changeDescription :: String -> a -> a

instance Describable (ScenarioDescription a) where
  changeDescription d sc = sc { sDesc = d }

instance Describable ParameterDescription where
  changeDescription d pa = pa { pDesc = d }

scenario :: String -> State (ScenarioDescription m) () -> ScenarioDescription m
scenario name f = execState f sc0
  where sc0 = SDesc name "" M.empty M.empty

describe :: Describable a => String -> State a ()
describe desc = modify (changeDescription desc)

parameter :: String -> State ParameterDescription () -> State (ScenarioDescription m) ()
parameter name f = modify (addParam name param)
  where addParam k v sc0 = sc0 { sParams = M.insert k v (sParams sc0) }
        param = execState f param0
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

setup :: Step m () -> State (ScenarioDescription m) ()
setup = appendHook "setup"

run :: Step m () -> State (ScenarioDescription m) ()
run = appendHook "run"

teardown :: Step m () -> State (ScenarioDescription m) ()
teardown  = appendHook "teardown"

recover :: Step m () -> State (ScenarioDescription m) ()
recover  = appendHook "recover"

analyze :: Step m () -> State (ScenarioDescription m) ()
analyze = appendHook "analyze"

appendHook :: String -> Step m () -> State (ScenarioDescription m) ()
appendHook name f = modify (addHook name $ Action f)
  where addHook k v sc0 = sc0 { sHooks = M.insert k v (sHooks sc0) }

result :: Monad m => String -> Step m (Result m)
result name = do 
  (b,r) <- ask
  bResult b r name

logger :: Monad m => Step m (LogHandler m)
logger = ask >>= uncurry bLogger

writeResult :: Monad m => String -> String -> Step m ()
writeResult name dat = result name >>= flip pWrite dat

appendResult :: Monad m => String -> String -> Step m ()
appendResult name dat = result name >>= flip pAppend dat

dbg :: Monad m => String -> Step m ()
dbg msg = logger >>= flip lLog msg

param :: Monad m => String -> Step m ParameterValue
param key = do
    ret <- liftM (M.lookup key . eParamSet . snd) ask
    maybe (throwError $ ExecutionError $ "missing param: " ++ key) return ret

getVar' :: (Functor m, MonadState DynEnv m) => String -> m (Maybe Dynamic)
getVar' k = M.lookup k <$> get

setVar' :: (MonadState DynEnv m) => String -> Dynamic -> m ()
setVar' k v = modify (M.insert k v)

setVar :: (Typeable v, MonadState DynEnv m) => String -> v -> m ()
setVar k v = setVar' k (toDyn v)

getVar :: (Typeable v, Functor m, MonadState DynEnv m) => String -> m (Maybe v)
getVar k = maybe Nothing fromDynamic <$> getVar' k

{- 
 - Example
 -}

vals :: ParameterSet
vals = M.fromList [("destination",str "probecraft.net"),("packet size",num 32)]

ping :: ScenarioDescription EnvIO
ping = scenario "ping" $ do
  describe "ping to a remote server"
  parameter "destination" $ do
    describe "a destination server (host or ip)"
    values [str "example.com", str "probecraft.net"]
  parameter "packet size" $ do
    describe "packet size in bytes"
    values [num 50, num 1500] 
  setup $ do
      setVar "hello" ("world"::String)
      dbg "setting up scenario"
      writeResult "foobar" "bla"
      dbg "setup action"
  run $ do
    (Just str :: Maybe String) <- getVar "hello"
    liftIO . print . ("hello "++) $ str
    (StringParam srv) <- param "destination"
    dbg $ "sending ping to " ++ srv
  teardown $ dbg "teardown action"
  recover $ dbg "recovering from error"
  analyze $ dbg "analyze action"
