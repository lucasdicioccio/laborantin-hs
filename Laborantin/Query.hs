{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Laborantin.Query where

import Laborantin.Types
import qualified Data.Map as M
import Control.Applicative ((<$>),(<*>),(*>),(<*))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Token
import Data.Maybe

type Param = Maybe ParameterValue

data EvalError = EvalError String
    deriving (Show)

matchQExpr :: QExpr Bool -> Execution m -> Bool
matchQExpr e q = match' (evalExpr q e)
    where match' (Right True) = True
          match' _            = False

evalExpr :: Execution m -> QExpr a -> Either EvalError a
evalExpr _ (N x)              = Right x
evalExpr _ (B x)              = Right x
evalExpr _ (S x)              = Right x
evalExpr _ (L x)              = Right x
evalExpr _ (T x)              = Right x
evalExpr exec ScName          = Right $ sName $ eScenario exec
evalExpr exec ScStatus | eStatus exec == Success = Right "success"
                       | eStatus exec == Failure = Right "failure"
                       | eStatus exec == Running = Right "running"
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

coerceStringParam :: Text -> Param -> Either EvalError (Text)
coerceStringParam name (Just (StringParam str)) = Right str
coerceStringParam name _ = Left (EvalError $    "could not coerce "
                                             ++ T.unpack name
                                             ++ " to String")

coerceNumberParam :: Text -> Param -> Either EvalError (Rational)
coerceNumberParam name (Just (NumberParam r)) = Right r
coerceNumberParam name _ = Left (EvalError $ "could not coerce "++ T.unpack name ++" to number")

showExpr :: QExpr a -> String
showExpr (N x) = show x
showExpr (B x) = show x
showExpr (S x) = show x
showExpr (L x) = show x
showExpr (T x) = show x
showExpr (Not x)  = "! " ++ "(" ++ showExpr x ++ ")"
showExpr (And e1 e2)        = "(" ++ showExpr e1 ++ " && " ++ showExpr e2 ++ ")"
showExpr (Or e1 e2)         = "(" ++ showExpr e1 ++ " || " ++ showExpr e2 ++ ")"
showExpr (Contains e1 e2)   = "(" ++ showExpr e1 ++ " in " ++ showExpr e2 ++ ")"
showExpr (Gt e1 e2)         = "(" ++ showExpr e1 ++ " >= " ++ showExpr e2 ++ ")"
showExpr (Eq e1 e2)         = "(" ++ showExpr e1 ++ " == " ++ showExpr e2 ++ ")"
showExpr (Plus e1 e2)       = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Times e1 e2)      = "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"
showExpr ScName          = "@sc.name"
showExpr ScStatus        = "@sc.status"
showExpr (ScParam key)   = "@sc.param:" ++ show key
showExpr (SCoerce x) = showExpr x
showExpr (NCoerce x) = showExpr x

instance (Show (QExpr a)) where
    show = showExpr

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

expr :: Parsec String u UExpr
expr = foldl chainl1 term [try inclOp, try mulOp, try addOp, try compOp, try boolOp]
    where term = expr' <|> negated <|> ary <|> literal <|> special
                 where expr' = char '(' *> expr <* char ')'

negated :: Parsec String u UExpr
negated = UNot <$> (spaces *> char '!' *> spaces *> expr)


ary :: Parsec String u UExpr
ary = UL <$> (char '[' *> (expr `sepBy` (spaces >> char ',' >> spaces)) <* char ']')
      
binOp xs = do
    spaces
    val <- foldl1 (<|>) (map (try . string . fst) xs)
    spaces
    return $ fromJust (lookup val xs)

addOp  = binOp [("+",UPlus),("-",UMinus)]
mulOp  = binOp [("*",UTimes),("/",UDiv)]
boolOp = binOp [("and",UAnd),("or",UOr)]
compOp = binOp [(">=",UGte),(">",UGt),("==",UEq),("<=",ULte),("<",ULt)]
inclOp = binOp [("in",UContains), ("~>",UContains)]

literal :: Parsec String u UExpr
literal = bool <|> (UN <$> number) <|> (US . T.pack <$> quotedString)

special :: Parsec String u UExpr
special = try scname <|> try scstatus <|> scparam

scname,scstatus,scparam :: Parsec String u UExpr
scname = string "@sc.name" *> return UScName
scstatus = string "@sc.status" *> return UScStatus
scparam = UScParam . T.pack <$> (syntax1 <|> syntax2)
    where syntax1 = string "@sc.param" *> spaces *> quotedString
          syntax2 = char ':' *> plainString

quotedString :: Parsec String u String
quotedString = char '"' *> many (noneOf "\"") <* char '"'

plainString :: Parsec String u String
plainString = many (noneOf " ")

number :: Parsec String u (Rational)
number = do
    (dec,frac) <- (try decFrac) <|> (try dec)
    return $ read (dec ++ frac ++ " % 1" ++ (map snd $ zip frac (repeat '0')))

dec,decFrac :: Parsec String u (String,String)
dec     = (,"") <$> many1 digit
decFrac = do  a <- many1 digit
              char '.'
              b <- many1 digit
              return (a,b)

bool :: Parsec String u UExpr
bool = true <|> false <|> fail "bool"
    where true = string "true" >> return (UB True) 
          false = string "false" >> return (UB False) 
