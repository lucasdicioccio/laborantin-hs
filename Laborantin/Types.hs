{-# LANGUAGE OverloadedStrings #-}
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
    ,   emptyScenario
    ,   emptyParameter
    ,   paramSets
    ,   mergeParamSpaces
    ,   updateParam
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
    ,   emptyEnv
    ,   TExpr (..)
    ,   UExpr (..)
    ,   Dependency (..)
) where

import qualified Data.Map as M
import Data.Time (UTCTime)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Data.Dynamic
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub)

-- | DynEnv is a map between Text keys and Dynamic values.
type DynEnv = M.Map Text Dynamic
emptyEnv :: DynEnv
emptyEnv = M.empty

-- | A ParameterSpace maps parameter names to their descriptions.
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


-- | A step is a stateful operation for a Scenario phase.
-- It carries a modifiable DynEnv between hooks and handle ExecutionErrors.
-- In addition, you can read (but not modify) the Backend and the Execution.
type Step m a = (ErrorT ExecutionError (StateT DynEnv (ReaderT (Backend m,Execution m) m)) a)

-- | An Action wraps a monadic computation inside a step.
newtype Action m = Action { unAction :: Step m () }

instance Show (Action m) where
  show _ = "(Action)"

instance Show (ExecutionError -> Action m) where
  show _ = "(Error-recovery action)"

-- | A Scenario description carries all information to run an experiment.
data ScenarioDescription m = SDesc {
    sName   :: Text
  , sDesc   :: Text
  , sParams :: ParameterSpace
  , sHooks  :: M.Map Text (Action m)
  , sRecoveryAction :: Maybe (ExecutionError -> Action m)
  , sDeps   :: [Dependency m]
  , sQuery  :: TExpr Bool
  } deriving (Show)

emptyScenario :: ScenarioDescription m
emptyScenario = SDesc "" "" M.empty M.empty Nothing [] noQuery

-- | A ParameterDescription description carries information for a single
-- parameter.
data ParameterDescription = PDesc {
    pName   :: Text
  , pDesc   :: Text
  , pValues :: [ParameterValue]
  } deriving (Show,Eq,Ord)

emptyParameter :: ParameterDescription
emptyParameter = PDesc "" "" []

-- | Two parameter values type should be enough for command-line demands: text
-- and numbers.
--
-- However, we provide two other constructors (Array and Range) for the
-- ParameterDescriptions in the DSL.
--
-- Executions should use text and numbers only.
data ParameterValue = StringParam Text 
  | NumberParam Rational
  | Array [ParameterValue]
  | Range Rational Rational Rational -- [from, to], by increment
  deriving (Show,Eq,Ord)

-- | A ParameterSet (slightly different from a ParameterSpace) is a mapping
-- between parameter names and a single ParameterValue.
--
-- You can see a ParameterSet as a datapoint within a (multidimensional)
-- ParameterSpace.
--
-- Thus, to keep things clearer, we recommend that executions use only text and
-- numbers as ParameterValues.
type ParameterSet = M.Map Text ParameterValue

data ExecutionStatus = Running | Success | Failure 
  deriving (Show,Read,Eq)

-- | An Execution represents an ongoing or past experiment result.
data Execution m = Exec {
    eScenario :: ScenarioDescription m
  , eParamSet :: ParameterSet
  , ePath     :: FilePath
  , eStatus   :: ExecutionStatus
  , eAncestors   :: [Execution m] 
  , eTimeStamps :: (UTCTime,UTCTime)
} deriving (Show)

-- | An StoredExecution is a stripped-down version of an Execution.
--
-- As it represents an experiment stored on disk, it does not need to carry the
-- ScenarioDescription object (otherwise it would become harder to create
-- instances such as FromJSON for Executions).
data StoredExecution = Stored {
    seParamSet :: ParameterSet
  , sePath     :: FilePath
  , seStatus   :: ExecutionStatus
  , seAncestors :: [(FilePath,Text)]
  , seTimeStamps :: (UTCTime,UTCTime)
} deriving (Show)

-- | A Dependency is a lose but flexible way of expressing dependencies for
-- experiments.
--
-- Dependencies can check whether they are fullfilled, and try to solve.
-- The main goal for the design of Dependency dCheck and dSolve hooks is to let
-- a Dependency run experiments and add them as ancestors *before* starting any
-- Step. Types may slightly vary in the future.
--
-- Dependencies can do anything that a ScenarioDescription allows (hence they
-- are parametrized with the same monad as the ScenarioDescription owning a
-- Dependency). However, Dependency check and Dependency resolution do not live
-- in a Step m . That is they do not have access to, and cannot modify, the
-- DynEnv. Thus, this limits the possibility to read execution parameters from
-- within the dCheck and dSolve.
--
-- To compensate for this limitation, the dCheck hook accepts the Execution as
-- parameter and the dSolve hook accepts both the Execution and the Backend as
-- parameter, and get a chance to return a modified Execution object.
data Dependency m = Dep {
      dName     :: Text
    , dDesc     :: Text
    , dCheck    :: Execution m -> m Bool
    , dSolve    :: (Execution m, Backend m) -> m (Execution m)
    }

instance Eq (Dependency m) where
    d1 == d2 = dName d1 == dName d2 && dDesc d1 == dDesc d2

instance Show (Dependency m) where
    show dep = "Dep {dName="
                ++ show (dName dep)
                ++ ", dDesc="
                ++ show (dDesc dep)
                ++ "}" 

-- | Expands a ParameterValue to a list of ParameterValues.
--  Mainly flattens ranges.
expandValue :: ParameterValue -> [ParameterValue]
expandValue (Range from to by)  = map NumberParam [from,from+by .. to]
expandValue x                   = [x]

-- | Returns an exhaustive list of ParameterSet (i.e., all data points) to
-- cover a (multidimensional) ParameterSpace.
--
-- Basically a Cartesian product.
paramSets :: ParameterSpace -> [ParameterSet]
paramSets ps = map M.fromList $ sequence possibleValues
    where possibleValues = map f $ M.toList ps
          f (k,desc) = concatMap (map (pName desc,) . expandValue) $ pValues desc
type Finalizer m = Execution m -> m ()

-- | Merges two ParameterSpace by extending all dimensions.
mergeParamSpaces :: ParameterSpace -> ParameterSpace -> ParameterSpace
mergeParamSpaces ps1 ps2 = M.mergeWithKey f id id ps1 ps2
    where f k v1 v2 = Just (v1 { pValues = values })
                        where values = nub $ (pValues v1) ++ (pValues v2)

-- | Updates a single dimension of the ParameterSpace to be the list of
-- ParameterValue s in 3rd parameter.
updateParam :: ParameterSpace -> Text -> [ParameterValue] -> ParameterSpace
updateParam ps key values = M.updateWithKey f key ps
    where f k param = Just (param {pValues = values})

-- | A Backend captures all functions that an object must provide to run
-- Laborantin experiments.
--
-- Such functions give ways to prepare, run, analyze, and finalize executions.
-- As well as provide support for logging info, storing,
-- finding, and deleting prior results.
--
-- We prefer such a design over a typeclass to simplify overall design and
-- unclutter type definitions everywhere.
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

-- | Backends must generate results that are easy to operate. They represent
-- files with read/write/append operations as execution steps.
--
-- Note that Backend might not implement all three of read, write, append
-- operations.
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

noQuery :: TExpr Bool
noQuery = B False

showTExpr :: TExpr a -> String
showTExpr (N x)             = show x
showTExpr (B x)             = show x
showTExpr (S x)             = show x
showTExpr (L x)             = show x
showTExpr (T x)             = "t:" ++ show x
showTExpr (Not x)           = "! " ++ "(" ++ showTExpr x ++ ")"
showTExpr (And e1 e2)       = "(" ++ showTExpr e1 ++ " && " ++ showTExpr e2 ++ ")"
showTExpr (Or e1 e2)        = "(" ++ showTExpr e1 ++ " || " ++ showTExpr e2 ++ ")"
showTExpr (Contains e1 e2)  = "(" ++ showTExpr e1 ++ " in " ++ showTExpr e2 ++ ")"
showTExpr (Gt e1 e2)        = "(" ++ showTExpr e1 ++ " >  " ++ showTExpr e2 ++ ")"
showTExpr (Eq e1 e2)        = "(" ++ showTExpr e1 ++ " == " ++ showTExpr e2 ++ ")"
showTExpr (Plus e1 e2)      = "(" ++ showTExpr e1 ++ " + " ++ showTExpr e2 ++ ")"
showTExpr (Times e1 e2)     = "(" ++ showTExpr e1 ++ " * " ++ showTExpr e2 ++ ")"
showTExpr ScName            = "@sc.name"
showTExpr ScStatus          = "@sc.status"
showTExpr ScTimestamp       = "@sc.timestamp"
showTExpr (ScParam key)     = "@sc.param:" ++ show key
showTExpr (SCoerce x)       = "str!{"++(showTExpr x)++"}"
showTExpr (NCoerce x)       = "num!{"++(showTExpr )x++"}"
showTExpr (SilentSCoerce x) = "str{"++(showTExpr x)++"}"
showTExpr (SilentNCoerce x) = "num{"++(showTExpr x)++"}"
showTExpr (TBind  str f x)  = "(" ++ str ++ " -> (" ++ showTExpr x ++ "))"

instance (Show (TExpr a)) where
    show = showTExpr

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

showUExpr :: UExpr -> String
showUExpr (UN x) = show x
showUExpr (UB x) = show x
showUExpr (US x) = show x
showUExpr (UL x) = show x
showUExpr (UT x)              = "t:" ++ show x
showUExpr (UNot x)            = "! " ++ "(" ++ showUExpr x ++ ")"
showUExpr (UAnd e1 e2)        = "(" ++ showUExpr e1 ++ " and " ++ showUExpr e2 ++ ")"
showUExpr (UOr e1 e2)         = "(" ++ showUExpr e1 ++ " or " ++ showUExpr e2 ++ ")"
showUExpr (UContains e1 e2)   = "(" ++ showUExpr e1 ++ " in " ++ showUExpr e2 ++ ")"
showUExpr (UGt e1 e2)         = "(" ++ showUExpr e1 ++ " > " ++ showUExpr e2 ++ ")"
showUExpr (UGte  e1 e2)       = "(" ++ showUExpr e1 ++ " >= " ++ showUExpr e2 ++ ")"
showUExpr (ULt e1 e2)         = "(" ++ showUExpr e1 ++ " < " ++ showUExpr e2 ++ ")"
showUExpr (ULte e1 e2)        = "(" ++ showUExpr e1 ++ " <= " ++ showUExpr e2 ++ ")"
showUExpr (UEq e1 e2)         = "(" ++ showUExpr e1 ++ " == " ++ showUExpr e2 ++ ")"
showUExpr (UPlus e1 e2)       = "(" ++ showUExpr e1 ++ " + " ++ showUExpr e2 ++ ")"
showUExpr (UMinus e1 e2)      = "(" ++ showUExpr e1 ++ " - " ++ showUExpr e2 ++ ")"
showUExpr (UTimes e1 e2)      = "(" ++ showUExpr e1 ++ " * " ++ showUExpr e2 ++ ")"
showUExpr (UDiv  e1 e2)       = "(" ++ showUExpr e1 ++ " / " ++ showUExpr e2 ++ ")"
showUExpr UScName          = "@sc.name"
showUExpr UScStatus        = "@sc.status"
showUExpr UScTimestamp     = "@sc.timestamp"
showUExpr (UScParam key)   = "@sc.param:" ++ show key

instance (Show UExpr) where
    show = showUExpr
