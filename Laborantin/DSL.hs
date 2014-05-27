{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Laborantin.DSL (
        scenario
    ,   describe
    ,   parameter
    ,   produces
    ,   consumes
    ,   require
    ,   requireTExpr
    ,   dependency
    ,   check
    ,   resolve
    ,   values
    ,   str
    ,   num
    ,   range
    ,   arr
    ,   setup
    ,   teardown
    ,   run
    ,   self
    ,   backend
    ,   param
    ,   ancestors
    ,   ancestorsMatching
    ,   ancestorsMatchingTExpr
    ,   getVar
    ,   setVar
    ,   recover
    ,   analyze
    ,   consume
    ,   produce
    ,   writeResult
    ,   appendResult
    ,   logger
    ,   dbg
    ,   err
) where

import qualified Data.Map as M
import Data.List (partition, nubBy)
import Laborantin
import Laborantin.Types
import Laborantin.Query
import Laborantin.Query.Parse
import Laborantin.Query.Interpret
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Applicative
import Data.Dynamic
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

class Describable a where
  changeDescription :: Text -> a -> a

instance Describable (ScenarioDescription a) where
  changeDescription d sc = sc { sDesc = d }

instance Describable ParameterDescription where
  changeDescription d pa = pa { pDesc = d }

instance Describable (Dependency a) where
  changeDescription d dep = dep { dDesc = d }

-- | DSL entry point to build a 'ScenarioDescription'.
scenario :: (Monad m) => Text -> State (ScenarioDescription m) () -> ScenarioDescription m
scenario name f = execState f sc0
  where sc0 = emptyScenario { sName = name, sHooks = defaultNoHooks }  
                where defaultNoHooks = M.fromList [ ("setup", noop)
                                                  , ("teardown", noop)
                                                  , ("run", noop)
                                                  , ("analyze", noop)
                                                  ]
                      noop = Action (return ())

-- | Attach a description to the 'Parameter' / 'Scnario'
describe :: Describable a => Text -> State a ()
describe desc = modify (changeDescription desc)

-- | DSL entry point to build a 'ParameterDescription' within a scenario.
parameter :: Text -> State ParameterDescription () -> State (ScenarioDescription m) ()
parameter name f = modify (addParam name param)
  where addParam k v sc0 = sc0 { sParams = M.insert k v (sParams sc0) }
        param = execState f param0
                where param0 = PDesc name "" []

-- | DSL entry point to build a 'Dependency a' within a scenario.
dependency :: (Monad m) => Text -> State (Dependency m) () -> State (ScenarioDescription m) ()
dependency name f = modify (addDep dep)
  where addDep v sc0 = sc0 { sDeps = v:(sDeps sc0) }
        dep = execState f dep0
              where dep0 = Dep name "" checkF solveF
                    checkF = const (return True)
                    solveF = return . fst

-- | Set verification action for the dependency
check :: (Execution m -> m Bool) -> State (Dependency m) ()
check f = do
  dep0 <- get
  put $ dep0 { dCheck = f }

-- | Set resolution action for the dependency
resolve :: ((Execution m, Backend m) -> m (Execution m)) -> State (Dependency m) ()
resolve f = do
  dep0 <- get
  put $ dep0 { dSolve = f }

-- | Set default values for the paramater
values :: [ParameterValue] -> State ParameterDescription ()
values xs = do
  param0 <- get
  put $ param0 { pValues = xs }

-- | Encapsulate a Text as a 'ParameterValue'
str :: Text -> ParameterValue
str = StringParam

-- | Encapsulate an integer value as a 'ParameterValue'
num :: Integer -> ParameterValue
num = NumberParam . fromInteger

-- | Encapsulate a range as a 'ParameterValue'
range :: Rational -> Rational -> Rational -> ParameterValue
range = Range

-- | Encapsulate an array of 'str' or 'num' values as a 'ParameterValue'
arr :: [ParameterValue] -> ParameterValue
arr = Array

-- | Define the setup hook for this scenario
setup :: Step m () -> State (ScenarioDescription m) ()
setup = appendHook "setup"

-- | Define the main run hook for this scenario
run :: Step m () -> State (ScenarioDescription m) ()
run = appendHook "run"

-- | Define the teardown hook for this scenario
teardown :: Step m () -> State (ScenarioDescription m) ()
teardown  = appendHook "teardown"

-- | Define the recovery hook for this scenario
recover :: (ExecutionError -> Step m ()) -> State (ScenarioDescription m) ()
recover f = modify (setRecoveryAction action)
  where action err = Action (f err)
        setRecoveryAction act sc = sc { sRecoveryAction = Just act }

-- | Define the offline analysis hook for this scenario
analyze :: Step m () -> State (ScenarioDescription m) ()
analyze = appendHook "analyze"

appendHook :: Text -> Step m () -> State (ScenarioDescription m) ()
appendHook name f = modify (addHook name $ Action f)
  where addHook k v sc0 = sc0 { sHooks = M.insert k v (sHooks sc0) }

-- | Defines the TExpr Bool to load ancestor
requireTExpr :: (MonadIO m, Monad m) => ScenarioDescription m -> TExpr Bool -> State (ScenarioDescription m) ()
requireTExpr sc query = do
    let depName = T.concat [(sName sc),  " ~> ", (pack $ show query)]
    modify (\sc0 -> sc0 {sQuery = query})
    dependency depName $ do
        describe "auto-generated dependency for `require` statement"
        check $ \exec -> do
            let ancestors = filter ((sName sc ==) . sName . eScenario) (eAncestors exec)
            let existing = map eParamSet ancestors
            let missing = missingParameterSets sc query existing
            return (null $ missing)
        resolve $ \(exec,backend) -> do
            storedAncestors <- load backend [sc] query
            let (execAncestors, otherAncestors) = partition ((sName sc ==) . sName . eScenario) (eAncestors exec)
            let allAncestors = nubBy (samePath) (storedAncestors ++ execAncestors)
            newAncestors <- sequence $ prepare backend query allAncestors sc
            return (exec { eAncestors = newAncestors ++ allAncestors })  
            where samePath e1 e2 = ePath e1 == ePath e2

-- | Defines the TExpr Bool to load ancestor
require :: (MonadIO m, Monad m) => ScenarioDescription m -> Text -> State (ScenarioDescription m) ()
require sc txt = requireTExpr sc query
    where query = either (const deflt) (toTExpr deflt) (parseUExpr defaultParsePrefs (unpack txt))
          deflt = (B True)

-- | Declare that the scenario produces a result
produces :: (MonadIO m, Monad m)
  => FilePath
  -> State (ScenarioDescription m) (ResultDescription Produced)
produces resname = do
  let result = (RDesc resname)
  modify (add result)
  return result
  where add x sc@(SDesc {sProduced = xs}) = sc { sProduced = x:xs }

-- | Declare that the scenario produces a result
consumes :: (MonadIO m, Monad m)
  => FilePath
  -> State (ScenarioDescription m) (ResultDescription Consumed)
consumes resname = do
  let result = (RDesc resname)
  modify (add result)
  return result
  where add x sc@(SDesc {sConsumed = xs}) = sc { sConsumed = x:xs }


-- | Returns current execution
self :: Monad m => Step m (Execution m)
self = liftM snd ask

-- | Returns current backend
backend :: Monad m => Step m (Backend m)
backend = liftM fst ask

-- | Returns a consumable 'Result' object for the given result description.
consume :: Monad m => ResultDescription Consumed -> Step m (Result m Consumed)
consume desc = do 
  (b,r) <- ask
  bConsume b r desc

-- | Returns a produceable 'Result' object for the given result description.
produce :: Monad m => ResultDescription Produced -> Step m (Result m Produced)
produce desc = do 
  (b,r) <- ask
  bProduce b r desc

-- | Write (overwrite) the result in its entirety.
--
-- Implementations will return their specific results.
writeResult :: Monad m => ResultDescription Produced  -- ^ result produced
                       -> Text  -- ^ result content
                       -> Step m ()
writeResult desc dat = produce desc >>= \(ProducedResult _ write) -> write dat

-- | Appends a chunk of data to the result. 
--
-- Implementations will return their specific results.
appendResult :: Monad m => ResultDescription Produced -- ^ result produced
                        -> Text -- ^ content to add
                        -> Step m ()
appendResult desc dat = produce desc >>= \(ProducedResult append _) -> append dat

-- | Return a 'LogHandler' object for this scenario.
logger :: Monad m => Step m (LogHandler m)
logger = ask >>= uncurry bLogger

-- | Sends a line of data to the logger (debug mode)
dbg :: Monad m => Text -> Step m ()
dbg msg = logger >>= flip lLog msg

-- | Interrupts the scenario by throwing an error
err :: Monad m => String -> Step m ()
err = throwError . ExecutionError

-- | Get the parameter with given name.
-- Throw an error if the parameter is missing.
param :: Monad m => Text -- ^ the parameter name
                 -> Step m ParameterValue
param key = do
    ret <- liftM (M.lookup key . eParamSet . snd) ask
    maybe (throwError $ ExecutionError $ "missing param: " ++ unpack key) return ret

getVar' :: (Functor m, MonadState DynEnv m) => Text -> m (Maybe Dynamic)
getVar' k = M.lookup k <$> get

setVar' :: (MonadState DynEnv m) => Text -> Dynamic -> m ()
setVar' k v = modify (M.insert k v)

-- | Set an execution variable.
setVar :: (Typeable v, MonadState DynEnv m) =>
            Text -- ^ name of the variable
         -> v      -- ^ value of the variable
         -> m ()
setVar k v = setVar' k (toDyn v)

-- | Get an execution variable and tries to cast it from it's Dynamic
-- representation.
--
-- Returns 'Nothing' if the variable is missing or if it could not
-- be cast to the wanted type.
getVar :: (Typeable v, Functor m, MonadState DynEnv m) => 
            Text              -- ^ name of the variable
         -> m (Maybe v)      
getVar k = maybe Nothing fromDynamic <$> getVar' k

-- | Get all ancestors for a given scenario name and matching a TExpr Bool query.
ancestorsMatchingTExpr :: (Monad m) => Text -> TExpr Bool -> Step m [Execution m]
ancestorsMatchingTExpr name query = liftM (matching . eAncestors . snd) ask
    where matching = filter (matchTExpr query)

-- | Get all ancestors for a given scenario name and matching a query expressed as a string.
-- Current implementation silences errors.
ancestorsMatching :: (Monad m) => Text -> Text -> Step m [Execution m]
ancestorsMatching name txt = ancestorsMatchingTExpr name query
    where query = either (const deflt) (toTExpr deflt) (parseUExpr defaultParsePrefs (unpack txt))
          deflt = (B False)

-- | Get all ancestors for a given scenario name.
ancestors :: (Monad m) => Text -> Step m [Execution m]
ancestors = flip ancestorsMatchingTExpr (B True)
