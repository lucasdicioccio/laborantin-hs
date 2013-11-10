{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Laborantin.Types (
        ScenarioDescription (..)
    ,   ParameterDescription (..)
    ,   ParameterValue (..)
    ,   ParameterSpace
    ,   ParameterSet
    ,   paramSets
    ,   expandValue
    ,   Result (..)
    ,   Backend (..)
    ,   Execution (..)
    ,   StoredExecution (..)
    ,   ExecutionError (..)
    ,   AnalysisError (..)
    ,   ExecutionStatus (..)
    ,   Finalizer (..)
    ,   LogHandler (..)
    ,   Step
    ,   Action (..)
    ,   DynEnv (..)
    ,   QExpr (..)
) where

import qualified Data.Map as M
import System.Time(ClockTime)
import Control.Monad.Reader
import Control.Monad.Error
import Data.Dynamic

type DynEnv = M.Map String Dynamic
type ParameterSpace = M.Map String ParameterDescription
data ExecutionError = ExecutionError String
    deriving (Show)
data AnalysisError = AnalysisError String
    deriving (Show)
instance Error ExecutionError where
  noMsg    = ExecutionError "A String Error!"
  strMsg   = ExecutionError
instance Error AnalysisError where
  noMsg    = AnalysisError "A String Error!"
  strMsg   = AnalysisError
type Step m a = ErrorT ExecutionError (ReaderT (Backend m,Execution m) m) a

newtype Action m = Action { unAction :: Step m () }

instance Show (Action m) where
  show _ = "(Action)"

instance Show (ExecutionError -> Action m) where
  show _ = "(Error-recovery action)"

data ScenarioDescription m = SDesc {
    sName   :: String
  , sDesc   :: String
  , sParams :: ParameterSpace
  , sHooks  :: M.Map String (Action m)
  , sRecoveryAction :: Maybe (ExecutionError -> Action m)
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
  deriving (Show,Read,Eq)

data Execution m = Exec {
    eScenario :: ScenarioDescription m
  , eParamSet :: ParameterSet
  , ePath     :: String
  , eStatus   :: ExecutionStatus
  , eAncestors   :: [Execution m] 
} deriving (Show)

data StoredExecution = Stored {
    seParamSet :: ParameterSet
  , sePath     :: String
  , seStatus   :: ExecutionStatus
  , seAncestors :: [(String, String)]
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
  , bRecover   :: ExecutionError -> Execution m -> Step m ()
  , bResult    :: Execution m -> String -> Step m (Result m)
  , bLoad      :: [ScenarioDescription m] -> QExpr Bool -> m [Execution m]
  , bLogger    :: Execution m -> Step m (LogHandler m)
  , bRemove    :: Execution m -> m ()
}

data Result m = Result {
    pPath   :: String
  , pRead   :: Step m String
  , pAppend :: String -> Step m ()
  , pWrite  :: String -> Step m ()
}

newtype LogHandler m = LogHandler { lLog :: String -> Step m () }

data QExpr :: * -> * where
    N           :: (Show n, Num n) => n -> QExpr n
    B           :: Bool -> QExpr Bool
    S           :: String -> QExpr String
    L           :: (Show a) => [a] -> QExpr [a]
    T           :: ClockTime -> QExpr ClockTime
    Plus        :: (Show n, Num n) => QExpr n -> QExpr n -> QExpr n
    Times       :: (Show n, Num n) => QExpr n -> QExpr n -> QExpr n
    And         :: QExpr Bool -> QExpr Bool -> QExpr Bool
    Or          :: QExpr Bool -> QExpr Bool -> QExpr Bool
    Not         :: QExpr Bool -> QExpr Bool
    Contains    :: (Show a, Eq a)  => QExpr a -> QExpr [a] -> QExpr Bool
    Eq          :: (Show a, Eq a)  => QExpr a -> QExpr a -> QExpr Bool
    Gt          :: (Show a, Ord a) => QExpr a -> QExpr a -> QExpr Bool
    ScName      :: QExpr String
    ScParam     :: String -> QExpr (String, Maybe ParameterValue)
    SCoerce     :: QExpr (String, Maybe ParameterValue) -> QExpr String
    NCoerce     :: QExpr (String, Maybe ParameterValue) -> QExpr Rational
