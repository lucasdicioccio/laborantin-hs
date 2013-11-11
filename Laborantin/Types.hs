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
    ,   Dependency (..)
) where

import qualified Data.Map as M
import System.Time (ClockTime)
import Control.Monad.Reader
import Control.Monad.Error
import Data.Dynamic
import Data.Text (Text)
import qualified Data.Text as T

type DynEnv = M.Map Text Dynamic
type ParameterSpace = M.Map Text ParameterDescription
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
    sName   :: Text
  , sDesc   :: Text
  , sParams :: ParameterSpace
  , sHooks  :: M.Map Text (Action m)
  , sRecoveryAction :: Maybe (ExecutionError -> Action m)
  , sDeps   :: [Dependency m]
  } deriving (Show)

data ParameterDescription = PDesc {
    pName   :: Text
  , pDesc   :: Text
  , pValues :: [ParameterValue]
  } deriving (Show,Eq,Ord)

data ParameterValue = StringParam Text 
  | NumberParam Rational
  | Array [ParameterValue]
  | Range Rational Rational Rational -- [from, to], by increment
  deriving (Show,Eq,Ord)

type ParameterSet = M.Map Text ParameterValue

data ExecutionStatus = Running | Success | Failure 
  deriving (Show,Read,Eq)

data Execution m = Exec {
    eScenario :: ScenarioDescription m
  , eParamSet :: ParameterSet
  , ePath     :: FilePath
  , eStatus   :: ExecutionStatus
  , eAncestors   :: [Execution m] 
  , eTimeStamps :: (ClockTime,ClockTime)
} deriving (Show)

data StoredExecution = Stored {
    seParamSet :: ParameterSet
  , sePath     :: FilePath
  , seStatus   :: ExecutionStatus
  , seAncestors :: [(FilePath,Text)]
  , seTimeStamps :: (ClockTime,ClockTime)
} deriving (Show)

data Dependency m = Dep {
      dName     :: Text
    , dDesc     :: Text
    , dCheck    :: Execution m -> m Bool
    , dSolve    :: Execution m -> m ()
    }

instance Eq (Dependency m) where
    d1 == d2 = dName d1 == dName d2 && dDesc d1 == dDesc d2

instance Show (Dependency m) where
    show dep = "Dep {dName="
                ++ show (dName dep)
                ++ ", dDesc="
                ++ show (dDesc dep)
                ++ "}" 

expandValue :: ParameterValue -> [ParameterValue]
expandValue (Range from to by)  = map NumberParam [from,from+by .. to]
expandValue x                   = [x]

paramSets :: ParameterSpace -> [ParameterSet]
paramSets ps = map M.fromList $ sequence possibleValues
    where possibleValues = map f $ M.toList ps
          f (k,desc) = concatMap (map (pName desc,) . expandValue) $ pValues desc
type Finalizer m = Execution m -> m ()

data Backend m = Backend {
    bName      :: Text
  , bPrepareExecution  :: ScenarioDescription m -> ParameterSet -> m (Execution m,Finalizer m)
  , bFinalizeExecution :: Execution m -> Finalizer m -> m ()
  , bSetup     :: Execution m -> Step m ()
  , bRun       :: Execution m -> Step m ()
  , bTeardown  :: Execution m -> Step m ()
  , bAnalyze   :: Execution m -> Step m ()
  , bRecover   :: ExecutionError -> Execution m -> Step m ()
  , bResult    :: Execution m -> FilePath -> Step m (Result m)
  , bLoad      :: [ScenarioDescription m] -> QExpr Bool -> m [Execution m]
  , bLogger    :: Execution m -> Step m (LogHandler m)
  , bRemove    :: Execution m -> m ()
}

data Result m = Result {
    pPath   :: FilePath
  , pRead   :: Step m Text
  , pAppend :: Text -> Step m ()
  , pWrite  :: Text -> Step m ()
}

newtype LogHandler m = LogHandler { lLog :: Text -> Step m () }

data QExpr :: * -> * where
    N           :: (Show n, Num n) => n -> QExpr n
    B           :: Bool -> QExpr Bool
    S           :: Text -> QExpr Text
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
    ScName      :: QExpr Text
    ScStatus    :: QExpr Text
    ScParam     :: Text -> QExpr (Text, Maybe ParameterValue)
    SCoerce     :: QExpr (Text, Maybe ParameterValue) -> QExpr Text
    NCoerce     :: QExpr (Text, Maybe ParameterValue) -> QExpr Rational
