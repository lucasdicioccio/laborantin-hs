{-# LANGUAGE ExistentialQuantification #-}
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
    ,   mergeParamSpaces
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
    ,   TExpr (..)
    ,   UExpr (..)
    ,   Dependency (..)
) where

import qualified Data.Map as M
import Data.Time (UTCTime)
import Control.Monad.Reader
import Control.Monad.Error
import Data.Dynamic
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub)

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
  , eTimeStamps :: (UTCTime,UTCTime)
} deriving (Show)

data StoredExecution = Stored {
    seParamSet :: ParameterSet
  , sePath     :: FilePath
  , seStatus   :: ExecutionStatus
  , seAncestors :: [(FilePath,Text)]
  , seTimeStamps :: (UTCTime,UTCTime)
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

mergeParamSpaces :: ParameterSpace -> ParameterSpace -> ParameterSpace
mergeParamSpaces ps1 ps2 = M.mergeWithKey f id id ps1 ps2
    where f k v1 v2 = Just (v1 { pValues = values })
                        where values = nub $ (pValues v1) ++ (pValues v2)

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
  , bLoad      :: [ScenarioDescription m] -> TExpr Bool -> m [Execution m]
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

data TExpr :: * -> * where
    N           :: Rational -> TExpr Rational
    B           :: Bool -> TExpr Bool
    S           :: Text -> TExpr Text
    L           :: [TExpr a] -> TExpr [a]
    T           :: UTCTime -> TExpr UTCTime
    Plus        :: TExpr Rational -> TExpr Rational -> TExpr Rational
    Times       :: TExpr Rational -> TExpr Rational -> TExpr Rational
    And         :: TExpr Bool -> TExpr Bool -> TExpr Bool
    Or          :: TExpr Bool -> TExpr Bool -> TExpr Bool
    Not         :: TExpr Bool -> TExpr Bool
    Contains    :: (Show a, Eq a)  => TExpr a -> TExpr [a] -> TExpr Bool
    Eq          :: (Show a, Eq a)  => TExpr a -> TExpr a -> TExpr Bool
    Gt          :: (Show a, Ord a) => TExpr a -> TExpr a -> TExpr Bool
    ScName      :: TExpr Text
    ScStatus    :: TExpr Text
    ScParam     :: Text -> TExpr (Text, Maybe ParameterValue)
    ScTimestamp :: TExpr UTCTime
    SCoerce     :: TExpr (Text, Maybe ParameterValue) -> TExpr Text
    NCoerce     :: TExpr (Text, Maybe ParameterValue) -> TExpr Rational
    SilentSCoerce     :: TExpr (Text, Maybe ParameterValue) -> TExpr Text
    SilentNCoerce     :: TExpr (Text, Maybe ParameterValue) -> TExpr Rational
    TBind      :: String -> (a -> TExpr b) -> TExpr a -> TExpr b

data UExpr = UN Rational
    | UB Bool
    | US Text
    | UL [UExpr]
    | UT UTCTime
    | UPlus     UExpr UExpr
    | UMinus    UExpr UExpr
    | UTimes    UExpr UExpr
    | UDiv      UExpr UExpr
    | UAnd      UExpr UExpr
    | UOr       UExpr UExpr
    | UContains UExpr UExpr
    | UEq       UExpr UExpr
    | UGt       UExpr UExpr
    | UGte      UExpr UExpr
    | ULte      UExpr UExpr
    | ULt       UExpr UExpr
    | UNot UExpr
    | UScName
    | UScStatus
    | UScTimestamp
    | UScParam     Text
