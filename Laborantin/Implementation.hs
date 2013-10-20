{-# LANGUAGE OverloadedStrings #-}

module Laborantin.Implementation (
        EnvIO, runEnvIO
    , defaultBackend
    , defaultResult
    , defaultLog
) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C
import Laborantin.Types
import Data.Aeson (decode,encode,FromJSON,parseJSON,(.:),ToJSON,toJSON,(.=),object)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative ((<$>),(<*>))
import Data.List
import Data.Maybe
import Data.UUID
import System.Directory
import System.Random
import System.Log.Logger
import System.Log.Handler (close)
import System.Log.Handler.Simple
import System.Log.Handler.Log4jXML

-- | Default monad for 'defaultBackend'.
--   EnvIO carries a 'DynEnv' in a state and allows you to perform IO actions.
type EnvIO = (StateT DynEnv IO)

-- | Execute an EnvIO action in IO.
runEnvIO :: EnvIO a -> IO (a,DynEnv)
runEnvIO m = runStateT m M.empty

instance ToJSON ParameterValue where
    toJSON (StringParam str) = object ["type" .= ("string"::T.Text), "val" .= T.pack str]
    toJSON (NumberParam n)   = object ["type" .= ("num"::T.Text), "val" .= n]
    toJSON (Array xs)        = toJSON xs
    toJSON (Range _ _ _)     = error "should not have to encode ranges but concrete values instead"

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
              match "range"  = error "should not have to read ranges"
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

-- | Default backend for the 'EnvIO' monad.  This backend uses the filesystem
-- as storage and UUIDs for scenario instances (supposes that UUID collision
-- cannot happen).
--
-- Parameters, logfiles, and result data all are stored in a unique directory named
-- ./<scenario-name>/<uuid>
-- 
-- Results are individual files in this directory. There is no namespacing
-- hence avoid the following names: 'execution.json', 'execution-log.txt', and
-- 'execution-log.xml'. These three files are the scenario execution metadata
-- and logs.
--
defaultBackend :: Backend EnvIO
defaultBackend = Backend "default EnvIO backend" prepare finalize setup run teardown analyze recover result load log rm
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
        finalize  exec finalizer = do
                            finalizer exec
                            liftIO . putStrLn $ "execution finished\n"
                            liftIO $ BSL.writeFile (rundir ++ "/execution.json") (encode exec)
                            where rundir = ePath exec
        setup             = callHooks "setup" . eScenario
        run               = callHooks "run" . eScenario
        teardown          = callHooks "teardown" . eScenario
        analyze exec      = liftIO (putStrLn $ advertise exec) >> callHooks "analyze" (eScenario exec)
        recover err exec  = unAction (doRecover err)
                            where doRecover = fromMaybe (\_ -> Action $ return ()) (sRecoveryAction $ eScenario exec) 
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
        rm exec           = liftIO $ removeDirectoryRecursive $ ePath exec

        callHooks key sc  = maybe (error $ "no such hook: " ++ key) unAction (M.lookup key $ sHooks sc)
        advertise exec    = unlines [ "scenario: " ++ (show . sName . eScenario) exec
                                    ,   "rundir: " ++ ePath exec
                                    ,   "json-params: " ++ (C.unpack . encode . eParamSet) exec
                                    ]

-- | Default result handler for the 'EnvIO' monad (see 'defaultBackend').
defaultResult :: Execution m -> String -> Result EnvIO
defaultResult exec name = Result path read append write
  where read        = liftIO $ readFile path
        append dat  = liftIO $ appendFile path dat
        write dat   = liftIO $ writeFile path dat
        path        = intercalate "/" [ePath exec, name]

-- | Default logger for the 'EnvIO' monad (see 'defaultBackend').
defaultLog :: Execution m -> LogHandler EnvIO
defaultLog exec = LogHandler logF
    where logF msg = liftIO $ debugM (loggerName exec) msg
          path = ePath exec ++ "/execution.log"

loggerName :: Execution m -> String
loggerName exec = "laborantin:" ++ ePath exec
