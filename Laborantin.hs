
module Laborantin (
        prepare
    ,   load
    ,   remove
    ,   runAnalyze
    ,   missingParameterSets
    ,   expandTExprParams
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

prepare :: (MonadIO m)    => Backend m
                          -> TExpr Bool
                          -> [Execution m]
                          -> ScenarioDescription m
                          -> [m (Execution m)]
prepare b expr execs sc = map f (missingParameterSets expr execs sc)
          where f = execute b sc

missingParameterSets :: TExpr Bool -> [Execution m] -> ScenarioDescription m -> [ParameterSet]
missingParameterSets expr execs sc = filter matching $ listDiff target existing
    where existing  = map eParamSet $ filter ((== Success) . eStatus) execs
          target    = expandTExprParams expr sc
          matching  = matchTExpr' expr sc

-- | Expands parameters given a TExpr and a ScenarioDescription into a list of
-- parameter spaces (sort of cartesian product of all possible params)
expandTExprParams :: TExpr Bool -> ScenarioDescription m -> [ParameterSet]
expandTExprParams expr sc = paramSets $ expandParamSpace (sParams sc) expr

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

listDiff l1 l2 = S.toList (S.fromList l1 `S.difference` S.fromList l2)


load :: (MonadIO m) => Backend m -> [ScenarioDescription m] -> TExpr Bool -> m [Execution m]
load = bLoad

remove :: (MonadIO m) => Backend m -> Execution m -> m ()
remove = bRemove

runAnalyze :: (MonadIO m, Functor m) => Backend m -> Execution m -> m (Either AnalysisError ())
runAnalyze b exec = do
    let status = runReaderT (runStateT (runErrorT (go exec)) emptyEnv) (b, exec)
    (either rebrandError Right) . fst <$> status
    where go exec = bAnalyze b exec
          rebrandError (ExecutionError str) = Left $ AnalysisError str

