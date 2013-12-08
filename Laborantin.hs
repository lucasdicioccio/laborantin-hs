
module Laborantin (
        prepare
    ,   load
    ,   remove
    ,   runAnalyze
    ,   missingParameterSets
    ,   expandParameterSets
) where

import Laborantin.Types
import Laborantin.Query
import Laborantin.Implementation
import Control.Monad.IO.Class
import Control.Monad.Reader 
import Control.Monad.State 
import Control.Monad.Error
import Control.Applicative
import qualified Data.Set as S
import qualified Data.Map as M

-- | Prepare a list of execution actions for a given
-- (scenario, parameter-query, existing) ancestors.
--
-- This function returns one action per ParameterSet which is required by the
-- parameter-query and not yet present in the existing list of executions.
--
-- For instance, if the scenario has one parameter 'foo'; the query wants 'foo'
-- in [1,2,3,4]; and there is one execution with 'foo' == 2; then this function
-- returns 3 actions (for parameters 1, 3, and 4).
--
-- A user can then run these actions in sequence (or concurrently if it makes
-- sense).
prepare :: (MonadIO m)    => Backend m
                          -> TExpr Bool
                          -> [Execution m]
                          -> ScenarioDescription m
                          -> [m (Execution m)]
prepare b expr execs sc = map f (missingParameterSets sc expr existing)
          where f = execute b sc
                existing = map eParamSet execs

missingParameterSets :: ScenarioDescription m -> TExpr Bool -> [ParameterSet] -> [ParameterSet]
missingParameterSets sc expr sets = listDiff target sets
    where target = matchingParameterSets sc expr

matchingParameterSets :: ScenarioDescription m -> TExpr Bool -> [ParameterSet]
matchingParameterSets sc expr = filter matching allSets
    where matching = matchTExpr' expr sc
          allSets  = expandParameterSets sc expr 

-- | Expands parameters given a TExpr and a ScenarioDescription into a list of
-- parameter spaces (sort of cartesian product of all possible params)
expandParameterSets :: ScenarioDescription m -> TExpr Bool -> [ParameterSet]
expandParameterSets sc expr = paramSets $ expandParamSpace (sParams sc) expr

-- | Shortcut to remove from l1 the items in l2.
-- Implementation transposes objects to sets thus needs the Ord capability.
--
-- e.g. listDiff "abcd" "bcde" = "a"
listDiff :: Ord a => [a] -> [a] -> [a]
listDiff l1 l2 = S.toList (S.fromList l1 `S.difference` S.fromList l2)

-- | Executes a given scenario for a given set of parameters using a given backend.
execute :: (MonadIO m) => Backend m -> ScenarioDescription m -> ParameterSet -> m (Execution m)
execute b sc prm = execution
  where execution = do
            (exec,final) <- bPrepareExecution b sc prm 
            status <- liftM fst $ runReaderT (runStateT (runErrorT (go exec `catchError` recover exec)) emptyEnv) (b, exec)
            let exec' = either (\_ -> exec {eStatus = Failure}) (\_ -> exec {eStatus = Success}) status
            bFinalizeExecution b exec' final
            return exec'
            where go exec = do 
                        bSetup b exec
                        bRun b exec
                        bTeardown b exec
                        bAnalyze b exec
                  recover exec err = bRecover b err exec >> throwError err


-- | Loads executions of given ScenarioDescription and matching a given query
-- using a specific backend.
load :: (MonadIO m) => Backend m -> [ScenarioDescription m] -> TExpr Bool -> m [Execution m]
load = bLoad

-- | Remove an execution using a specific backend.
remove :: (MonadIO m) => Backend m -> Execution m -> m ()
remove = bRemove

-- | Runs the analysis hooks.
runAnalyze :: (MonadIO m, Functor m) => Backend m -> Execution m -> m (Either AnalysisError ())
runAnalyze b exec = do
    let status = runReaderT (runStateT (runErrorT (go exec)) emptyEnv) (b, exec)
    (either rebrandError Right) . fst <$> status
    where go exec = bAnalyze b exec
          rebrandError (ExecutionError str) = Left $ AnalysisError str

