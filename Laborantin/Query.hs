{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Laborantin.Query (matchTExpr, matchTExpr', simplifyOneBoolLevel, expandParamSpace) where

import Laborantin.Types
import qualified Data.Map as M
import Control.Applicative ((<$>),(<*>))
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T

type Param = Maybe ParameterValue

data EvalError = EvalError String
    deriving (Show)

simplifyOneBoolLevel :: TExpr Bool -> TExpr Bool
simplifyOneBoolLevel (And (B True) e)  = simplifyOneBoolLevel e
simplifyOneBoolLevel (And e (B True))  = simplifyOneBoolLevel e
simplifyOneBoolLevel (And a b)         = (And (simplifyOneBoolLevel a) (simplifyOneBoolLevel b))
simplifyOneBoolLevel (Or (B False) e)  = simplifyOneBoolLevel e
simplifyOneBoolLevel (Or e (B False))  = simplifyOneBoolLevel e
simplifyOneBoolLevel (Or a b)          = (Or (simplifyOneBoolLevel a) (simplifyOneBoolLevel b))
simplifyOneBoolLevel e                 = e

matchTExpr' :: TExpr Bool -> ScenarioDescription m -> ParameterSet -> Bool
matchTExpr' expr sc params = matchTExpr expr (Exec sc params "" Success [] (epoch, epoch)) 
    where epoch = error "should not evaluated time"

matchTExpr :: TExpr Bool -> Execution m -> Bool
matchTExpr e q = match' (evalExpr q e)
    where match' (Right True) = True
          match' _            = False

evalExpr :: Execution m -> TExpr a -> Either EvalError a
evalExpr exec (TBind _ f expr)    = evalExpr exec expr >>= evalExpr exec . f
evalExpr _ (N x)              = Right x
evalExpr _ (B x)              = Right x
evalExpr _ (S x)              = Right x
evalExpr exec (L xs)          = mapM (evalExpr exec) xs >>= Right
evalExpr _ (T x)              = Right x
evalExpr exec ScName          = Right $ sName $ eScenario exec
evalExpr exec ScTimestamp     = Right $ fst $ eTimeStamps exec
evalExpr exec ScStatus | eStatus exec == Success = Right "success"
                       | eStatus exec == Failure = Right "failure"
                       | eStatus exec == Running = Right "running"
evalExpr exec (ScParam key)   = Right $ (key, M.lookup key (eParamSet exec))
evalExpr x (Not e)            = not <$> evalExpr x e
evalExpr x (Gt e1 e2)         = (>=) <$> evalExpr x e1 <*> evalExpr x e2
evalExpr x (Eq e1 e2)         = (==) <$> evalExpr x e1 <*> evalExpr x e2
evalExpr x (Plus e1 e2)       = (+) <$> evalExpr x e1 <*> evalExpr x e2
evalExpr x (Times e1 e2)      = (*)  <$> evalExpr x e1 <*> evalExpr x e2
evalExpr x (And e1 e2)        = (&&) <$> evalExpr x e1 <*> evalExpr x e2
evalExpr x (Or e1 e2)         = (||) <$> evalExpr x e1 <*> evalExpr x e2
evalExpr x (SCoerce e1)       = evalExpr x e1 >>= uncurry coerceStringParam
evalExpr x (NCoerce e1)       = evalExpr x e1 >>= uncurry coerceNumberParam
evalExpr x (Contains (SilentSCoerce e1) e2)   = do
    paramVal <- (evalExpr x e1)
    case paramVal of
        (_, (Just (StringParam str)))   -> elem str <$> evalExpr x e2
        _                               -> return False
evalExpr x (Contains (SilentNCoerce e1) e2)   = do
    paramVal <- (evalExpr x e1)
    case paramVal of
        (_, (Just (NumberParam str)))   -> elem str <$> evalExpr x e2
        _                               -> return False
evalExpr x (Contains e1 e2)   = elem <$> evalExpr x e1 <*> evalExpr x e2


coerceStringParam :: Text -> Param -> Either EvalError (Text)
coerceStringParam _ (Just (StringParam str)) = Right str
coerceStringParam name _ = Left (EvalError $    "could not coerce "
                                             ++ T.unpack name
                                             ++ " to String")

coerceNumberParam :: Text -> Param -> Either EvalError (Rational)
coerceNumberParam name (Just (NumberParam r)) = Right r
coerceNumberParam name _ = Left (EvalError $ "could not coerce "++ T.unpack name ++" to number")

-- | Expands a ParameterSpace to all values that could match a TExpr Bool.
--
-- Currently only supports countably finite expansions of parameters. 
-- That is TExpr Bool such as (@sc.param "param" > 32) are ignored.
-- Instead (@sc.param "param" in ["foo", "bar"]) gets expanded to ("param", [StringParam "foo", StringParam "bar"])
-- Supported expensions are  And / Or / Eq / Contains.
--
-- The idea is that you can generate a list of Execution to run by first
-- expanding all possible points in the ParameterSpace modified by the TExpr,
-- and then filter these possible points using a same TExpr.
--
expandParamSpace :: ParameterSpace -> TExpr Bool -> ParameterSpace
expandParamSpace params query = case query of
    (Or expr1 expr2) -> mergeParamSpaces ps1 ps2
        where ps1 = expand expr1
              ps2 = expand expr2 
    (And expr1 expr2)  -> mergeParamSpaces ps1 ps2
        where ps1 = expand expr1
              ps2 = expand expr2 
    (Eq (SCoerce (ScParam key)) expr)               -> update key (toParamValues expr)
    (Eq (NCoerce (ScParam key)) expr)               -> update key (toParamValues expr)
    (Eq (SilentSCoerce (ScParam key)) expr)         -> update key (toParamValues expr)
    (Eq (SilentNCoerce (ScParam key)) expr)         -> update key (toParamValues expr)
    (Contains (SCoerce (ScParam key)) expr)         -> update key (toParamValues expr)
    (Contains (NCoerce (ScParam key)) expr)         -> update key (toParamValues expr)
    (Contains (SilentSCoerce (ScParam key)) expr)   -> update key (toParamValues expr)
    (Contains (SilentNCoerce (ScParam key)) expr)   -> update key (toParamValues expr)
    _   -> params

    where update = updateParam params
          expand = expandParamSpace params

-- | Interprets a `TExpr a` into a list of ParameterValue when it makes sense (i.e.,
-- on TExpr String / TExpr Rational / TExpr [String] / TExpr [Rational] )
toParamValues :: TExpr a -> [ParameterValue]
toParamValues (N x) = [NumberParam x]
toParamValues (S x) = [StringParam x]
toParamValues (L x) = concatMap toParamValues x
toParamValues _     = []
