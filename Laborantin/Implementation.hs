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
import Laborantin.Types
import Data.Aeson (decode,encode,FromJSON,parseJSON,(.:),ToJSON,toJSON,(.=),object)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Control.Monad.State
import Control.Applicative ((<$>),(<*>))
import Data.List
import Data.UUID
import System.Directory
import System.Random
import System.Log.Logger
import System.Log.Handler (close)
import System.Log.Handler.Simple
import System.Log.Handler.Log4jXML

type EnvIO = (StateT DynEnv IO)

runEnvIO :: EnvIO () -> IO ((), DynEnv)
runEnvIO m = runStateT m M.empty

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

loggerName :: Execution m -> String
loggerName exec = "laborantin:" ++ ePath exec
