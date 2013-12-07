{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Laborantin.Query.Interpret (toTExpr) where

import Laborantin.Types
import Control.Applicative ((<$>),(<*>))
import Laborantin.Query
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)

type Param = Maybe ParameterValue

data TTyp a where
    TTBool      :: TTyp Bool
    TTNum       :: TTyp Rational
    TTString    :: TTyp Text
    TTParam     :: TTyp (Text,Param)
    TTUTCTime   :: TTyp UTCTime

data IError = IError
    deriving (Show)

-- carries the type of a TExpr
data ATExpr = forall a . TExpr a ::: TTyp a

instance Show ATExpr where
    show (expr ::: ty) = show expr

toTExpr :: TExpr Bool -> UExpr -> (TExpr Bool)
toTExpr expr = fromMaybe expr . toTExpr'

toTExpr' :: UExpr -> Maybe (TExpr Bool)
toTExpr' u = case interpret u of
    Right (expr ::: TTBool) -> Just expr
    _                       -> Nothing


interpret :: UExpr -> Either IError ATExpr
interpret UScName       = Right (ScName ::: TTString)
interpret UScStatus     = Right (ScStatus ::: TTString)
interpret UScTimestamp  = Right (ScTimestamp ::: TTUTCTime)
interpret (UScParam a)  = Right (ScParam a ::: TTParam)
interpret (UB a)        = Right (B a ::: TTBool)
interpret (UT a)        = Right (T a ::: TTUTCTime)
interpret (UN a)        = Right (N a ::: TTNum)
interpret (US a)        = Right (S a ::: TTString)
interpret (UAnd a b)   = do
    vals <- ((,) <$> interpret a <*> interpret b)
    case vals of
        (x ::: TTBool, y ::: TTBool) -> Right (And x y ::: TTBool)
        _   -> error "expecting 'boolean' for And clause"
interpret (UOr a b)   = do
    vals <- ((,) <$> interpret a <*> interpret b)
    case vals of
        (x ::: TTBool, y ::: TTBool) -> Right (Or x y ::: TTBool)
        _   -> error "expecting 'boolean' for Or clause"
interpret (UPlus a b)   = do
    vals <- ((,) <$> interpret a <*> interpret b)
    case vals of
        (x ::: TTNum, y ::: TTNum) -> Right (Plus x y ::: TTNum)
        _   -> error "expecting 'number' for Plus operator"
interpret (UMinus a b) = do
    vals <- ((,) <$> interpret a <*> interpret b)
    case vals of
        (x ::: TTNum, y ::: TTNum) -> Right (Times x (TBind "0 - x" (\n -> N (0 - n)) y) ::: TTNum)
        _   -> error "expecting 'number' for Minus operator"
interpret (UTimes a b) = do
    vals <- ((,) <$> interpret a <*> interpret b)
    case vals of
        (x ::: TTNum, y ::: TTNum) -> Right (Times x y ::: TTNum)
        _   -> error "expecting 'number' for Times operator"
interpret (UDiv a b) = do
    vals <- ((,) <$> interpret a <*> interpret b)
    case vals of
        (x ::: TTNum, y ::: TTNum) -> Right (Times x (TBind "1/x" (\n -> N (1/n)) y) ::: TTNum)
        _   -> error "expecting 'number' for Div operator"
interpret (UEq a b) = do
    vals <- ((,) <$> interpret a <*> interpret b)
    case vals of
        (x ::: TTNum, y ::: TTNum)          -> Right (Eq x y ::: TTBool)
        (x ::: TTString, y ::: TTString)    -> Right (Eq x y ::: TTBool)
        (x ::: TTBool, y ::: TTBool)        -> Right (Eq x y ::: TTBool)
        (x ::: TTUTCTime, y ::: TTUTCTime)  -> Right (Eq x y ::: TTBool)
        (x ::: TTParam, y ::: TTString)     -> Right (Eq (SCoerce x) y ::: TTBool)
        (x ::: TTString, y ::: TTParam)     -> Right (Eq x (SCoerce y) ::: TTBool)
        (x ::: TTParam, y ::: TTNum)        -> Right (Eq (NCoerce x) y ::: TTBool)
        (x ::: TTNum, y ::: TTParam)        -> Right (Eq x (NCoerce y) ::: TTBool)
        (x ::: TTParam, y ::: TTParam)      -> Right (Eq x y ::: TTBool)
        _   -> error "type mismatch for equality check"
interpret (UGt a b) = do
    vals <- ((,) <$> interpret a <*> interpret b)
    case vals of
        (x ::: TTNum, y ::: TTNum)          -> Right (Gt x y ::: TTBool)
        (x ::: TTUTCTime, y ::: TTUTCTime)  -> Right (Gt x y ::: TTBool)
        (x ::: TTParam, y ::: TTNum)        -> Right (Gt (NCoerce x) y ::: TTBool)
        (x ::: TTNum, y ::: TTParam)        -> Right (Gt x (NCoerce y) ::: TTBool)
        (x ::: TTParam, y ::: TTParam)      -> Right (Gt (NCoerce x) (NCoerce y) ::: TTBool)
        _   -> error "type mismatch for comparison"
interpret (UGte a b) = interpret (UOr (UGt a b) (UEq a b))
interpret (ULt a b)  = interpret (UNot (UGte a b))
interpret (ULte a b) = interpret (UNot (UGt a b))
interpret (UContains a b) = do
    val <- interpret a
    case val of
        (x ::: TTNum)       -> Right (Contains x (L $ ttnums b) ::: TTBool)
        (x ::: TTUTCTime)   -> Right (Contains x (L $ ttutcs b) ::: TTBool)
        (x ::: TTString)    -> Right (Contains x (L $ ttstrs b) ::: TTBool)
        (x ::: TTParam)     -> Right ((Or (Contains (SilentSCoerce x) (L $ ttstrs b))
                                          (Contains (SilentNCoerce x) (L $ ttnums b)))
                                     ::: TTBool)
        _  -> error "interpretation unsupported for 'in'"
interpret (UL as) = do
    -- TODO evaluate non-heterogeneous lists
    error "cannot safely evaluate list which may be heterogeneous"
interpret (UNot x) = do
    val <- interpret x
    case val of
        (x ::: TTBool) -> Right (Not x ::: TTBool)
        _  -> error "expecting 'boolean' for Not clause"

ttnums :: UExpr -> [TExpr Rational]
ttnums (UL xs) = map (\(UN x) -> N x) (filter match xs)
    where match (UN _) = True
          match _      = False
ttnums _ = []

ttstrs :: UExpr -> [TExpr Text]
ttstrs (UL xs) = map (\(US x) -> S x) (filter match xs)
    where match (US _) = True
          match _      = False
ttstrs _ = []

ttutcs :: UExpr -> [TExpr UTCTime]
ttutcs (UL xs) = map (\(UT x) -> T x) (filter match xs)
    where match (UT _) = True
          match _      = False
ttutcs _ = []
