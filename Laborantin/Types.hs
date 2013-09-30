{-# LANGUAGE TupleSections #-}

module Laborantin.Types (
        ScenarioDescription (..)
    ,   ParameterDescription (..)
    ,   ParameterValue (..)
    ,   ParameterSpace
    ,   ParameterSet
    ,   paramSets
    ,   Result (..)
    ,   Backend (..)
    ,   Execution (..)
    ,   StoredExecution (..)
    ,   ExecutionError (..)
    ,   ExecutionStatus (..)
    ,   Finalizer (..)
    ,   LogHandler (..)
    ,   Step
    ,   Action (..)
    ,   DynEnv (..)
) where

import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Error
import Data.Dynamic

type DynEnv = M.Map String Dynamic
type ParameterSpace = M.Map String ParameterDescription
data ExecutionError = ExecutionError String
    deriving (Show)
instance Error ExecutionError where
  noMsg    = ExecutionError "A String Error!"
  strMsg   = ExecutionError
type Step m a = ErrorT ExecutionError (ReaderT (Backend m,Execution m) m) a

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
  | Range Rational Rational Rational -- [from, to], by increment
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

expandValue :: ParameterValue -> [ParameterValue]
expandValue (Range from to by)  = map NumberParam [from,from+by .. to]
expandValue x                   = [x]

paramSets :: ParameterSpace -> [ParameterSet]
paramSets ps = map M.fromList $ sequence possibleValues
    where possibleValues = map f $ M.toList ps
          f (k,desc) = concatMap (map (pName desc,) . expandValue) $ pValues desc
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
