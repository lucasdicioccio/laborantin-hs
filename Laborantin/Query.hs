{-# LANGUAGE GADTs #-}

module Laborantin.Query where

import Laborantin.Types

data Expr a where
    N           :: (Num n) => n -> Expr n
    B           :: Bool -> Expr Bool
    S           :: String -> Expr String
    L           :: [a] -> Expr [a]
    And         :: Expr Bool -> Expr Bool -> Expr Bool
    Or          :: Expr Bool -> Expr Bool -> Expr Bool
    Contains    :: (Eq a) => Expr a -> Expr [a] -> Expr Bool
    Lt          :: (Ord a) => Expr a -> Expr a -> Expr Bool
    Gt          :: (Ord a) => Expr a -> Expr a -> Expr Bool
    Eq          :: (Eq a) => Expr a -> Expr a -> Expr Bool

eval :: Expr a -> a
eval (N x)              = x
eval (B x)              = x
eval (S x)              = x
eval (L x)              = x
eval (And e1 e2)        = eval e1 && eval e2
eval (Or e1 e2)         = eval e1 || eval e2
eval (Contains e1 e2)   = eval e1 `elem` eval e2
eval (Lt e1 e2)         = eval e1 <= eval e2
eval (Gt e1 e2)         = eval e1 >= eval e2
eval (Eq e1 e2)         = eval e1 == eval e2

{-
 - End goal is to write queries such as
 -
 -   "select (sc.success && sc.name in ['foo','bar'] && sc.param 'bla' >= 32)"
 -
 - will need a parser
 - should extend Expr to express 'scenario/executions/paramvalues' in it so that we can
 - match existing scenarios executions as well as ask to run a set of scenarios
 - 
-}
