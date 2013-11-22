{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Laborantin.Query (matchTExpr, simplifyOneBoolLevel, showTExpr) where

import Laborantin.Types
import qualified Data.Map as M
import Control.Applicative ((<$>),(<*>))
import Data.Text (Text)
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

showTExpr :: TExpr a -> String
showTExpr (N x)             = show x
showTExpr (B x)             = show x
showTExpr (S x)             = show x
showTExpr (L x)             = show x
showTExpr (T x)             = show x
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
showTExpr (SCoerce x)       = showTExpr x
showTExpr (NCoerce x)       = showTExpr x
showTExpr (SilentSCoerce x) = showTExpr x
showTExpr (SilentNCoerce x) = showTExpr x
showTExpr (TBind  str f x)  = "(" ++ str ++ " -> (" ++ showTExpr x ++ "))"

instance (Show (TExpr a)) where
    show = showTExpr

showUExpr :: UExpr -> String
showUExpr (UN x) = show x
showUExpr (UB x) = show x
showUExpr (US x) = show x
showUExpr (UL x) = show x
showUExpr (UT x) = show x
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
showUExpr (UScParam key)   = "@sc.param:" ++ show key

instance (Show UExpr) where
    show = showUExpr
