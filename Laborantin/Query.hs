{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Laborantin.Query where

import Laborantin.Types
import System.Time(ClockTime)
import qualified Data.Map as M
import Control.Applicative ((<$>),(<*>))

data Expr :: * -> * where
    N           :: (Show n, Num n) => n -> Expr n
    B           :: Bool -> Expr Bool
    S           :: String -> Expr String
    L           :: (Show a) => [a] -> Expr [a]
    T           :: ClockTime -> Expr ClockTime
    Plus        :: (Show n, Num n) => Expr n -> Expr n -> Expr n
    Times       :: (Show n, Num n) => Expr n -> Expr n -> Expr n
    And         :: Expr Bool -> Expr Bool -> Expr Bool
    Or          :: Expr Bool -> Expr Bool -> Expr Bool
    Not         :: Expr Bool -> Expr Bool
    Contains    :: (Show a, Eq a)  => Expr a -> Expr [a] -> Expr Bool
    Eq          :: (Show a, Eq a)  => Expr a -> Expr a -> Expr Bool
    Gt          :: (Show a, Ord a) => Expr a -> Expr a -> Expr Bool
    ScName      :: Expr String
    ScParam     :: String -> Expr (String,Param)
    SCoerce     :: Expr (String, Param) -> Expr String
    NCoerce     :: Expr (String, Param) -> Expr Rational

type Param = Maybe ParameterValue
data EvalError = EvalError String
    deriving (Show)

evalExpr :: Execution m -> Expr a -> Either EvalError a
evalExpr _ (N x)              = Right x
evalExpr _ (B x)              = Right x
evalExpr _ (S x)              = Right x
evalExpr _ (L x)              = Right x
evalExpr _ (T x)              = Right x
evalExpr exec ScName          = Right $ sName $ eScenario exec
evalExpr exec (ScParam key)   = Right $ (key, M.lookup key (eParamSet exec))
evalExpr x (Not e)            = not <$> evalExpr x e
evalExpr x (Contains e1 e2)   = elem <$> evalExpr x e1 <*> evalExpr x e2
evalExpr x (Gt e1 e2)         = (>=) <$> evalExpr x e1 <*> evalExpr x e2
evalExpr x (Eq e1 e2)         = (==) <$> evalExpr x e1 <*> evalExpr x e2
evalExpr x (Plus e1 e2)       = (+) <$> evalExpr x e1 <*> evalExpr x e2
evalExpr x (Times e1 e2)      = (*)  <$> evalExpr x e1 <*> evalExpr x e2
evalExpr x (And e1 e2)        = (&&) <$> evalExpr x e1 <*> evalExpr x e2
evalExpr x (Or e1 e2)         = (||) <$> evalExpr x e1 <*> evalExpr x e2
evalExpr x (SCoerce e1)       = evalExpr x e1 >>= uncurry coerceStringParam
evalExpr x (NCoerce e1)       = evalExpr x e1 >>= uncurry coerceNumberParam

coerceStringParam :: String -> Param -> Either EvalError (String)
coerceStringParam name (Just (StringParam str)) = Right str
coerceStringParam name _ = Left (EvalError $ "could not coerce "++ name ++" to String")

coerceNumberParam :: String -> Param -> Either EvalError (Rational)
coerceNumberParam name (Just (NumberParam r)) = Right r
coerceNumberParam name _ = Left (EvalError $ "could not coerce "++ name ++" to number")

showExpr :: Expr a -> String
showExpr (N x) = show x
showExpr (B x) = show x
showExpr (S x) = show x
showExpr (L x) = show x
showExpr (T x) = show x
showExpr (Not x)  = "! " ++ showExpr x
showExpr (And e1 e2)        = "(" ++ showExpr e1 ++ " && " ++ showExpr e2 ++ ")"
showExpr (Or e1 e2)         = "(" ++ showExpr e1 ++ " || " ++ showExpr e2 ++ ")"
showExpr (Contains e1 e2)   = "(" ++ showExpr e1 ++ " in " ++ showExpr e2 ++ ")"
showExpr (Gt e1 e2)         = "(" ++ showExpr e1 ++ " >= " ++ showExpr e2 ++ ")"
showExpr (Eq e1 e2)         = "(" ++ showExpr e1 ++ " == " ++ showExpr e2 ++ ")"
showExpr (Plus e1 e2)       = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Times e1 e2)      = "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"
showExpr ScName          = "@sc.name"
showExpr (ScParam key)   = "@sc.param:" ++ key
showExpr (SCoerce x) = showExpr x
showExpr (NCoerce x) = showExpr x

{-
 - End goal is to write queries such as
 -
 -   "(sc.success && sc.name in ['foo','bar'] && sc.param 'bla' >= 32)"
 -
 - need a parser
 - support for heterogeneous list on CLI
 - maybe specify Expr with a ExprValue (nil/String/Number/Time) type
-}
