{-# LANGUAGE TupleSections #-}

module Laborantin.Query.Parse (
      expr
    , ParsePrefs (..)
    , defaultParsePrefs
    , parseUExpr
    ) where

import Laborantin.Types (UExpr (..))
import Laborantin.Query
import Control.Applicative ((<$>),(<*>),(*>),(<*))
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Data.Maybe
import System.Locale
import Data.Time
import Data.Time.Format

data ParsePrefs = ParsePrefs {
        prefTimeLocale :: TimeLocale
    } deriving  (Show)

defaultParsePrefs :: ParsePrefs
defaultParsePrefs = ParsePrefs defaultTimeLocale

parseUExpr :: ParsePrefs -> String -> Either ParseError UExpr
parseUExpr prefs str = runP expr prefs "-" str

expr :: Parsec String ParsePrefs UExpr
expr = foldl chainl1 term [try inclOp, try mulOp, try addOp, try compOp, try boolOp]
    where term = expr' <|> negated <|> ary <|> literal <|> special
                 where expr' = char '(' *> expr <* char ')'

negated :: Parsec String ParsePrefs UExpr
negated = UNot <$> (spaces *> char '!' *> spaces *> expr)

ary :: Parsec String ParsePrefs UExpr
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

literal :: Parsec String ParsePrefs UExpr
literal = try bool <|> date <|> (UN <$> number) <|> (US . T.pack <$> quotedString)

date :: Parsec String ParsePrefs UExpr
date = do
    string "t:"
    str <- quotedString
    timeLocale <- prefTimeLocale <$> getState
    let parsed = parseTimeFormats' timeLocale fmts str
    case parsed of
        [x] -> return $ UT x
        _   -> fail "invalid time format"
    where fmts =    [ iso8601DateFormat Nothing
                    , rfc822DateFormat
                    , iso8601DateFormat (Just "%T")
                    ]

parseTimeFormats' locale fmts str = take 1 . catMaybes $ parseResults
    where parseResults = map (\fmt -> parseTime locale fmt str) fmts

special :: Parsec String u UExpr
special = try scname <|> try scstatus <|> try sctimestamp <|> scparam

scname,scstatus,scparam,sctimestamp :: Parsec String u UExpr
scname = string "@sc.name" *> return UScName
scstatus = string "@sc.status" *> return UScStatus
scparam = UScParam . T.pack <$> (syntax1 <|> syntax2)
    where syntax1 = string "@sc.param" *> spaces *> quotedString
          syntax2 = char ':' *> plainString
sctimestamp = string "@sc.timestamp" *> return UScTimestamp

quotedString :: Parsec String u String
quotedString = try (char '"' *> many (noneOf "\"") <* char '"')
               <|> (char '\'' *> many (noneOf "'") <* char '\'')

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
