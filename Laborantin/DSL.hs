{-# LANGUAGE FlexibleContexts #-}

module Laborantin.DSL where

import qualified Data.Map as M
import Laborantin.Types
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Applicative
import Data.Dynamic

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

