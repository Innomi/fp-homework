{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Block2
  ( ArithmeticError (..)
  , eval
  , Expr (..)
  , Monad (..)
  , MonadFish (..)
  , MonadJoin (..)
  , moving
  ) where

import qualified Control.Monad.State as CMS (get, State, evalState, put, return)
import Numeric.Natural (Natural)
import Prelude hiding (Monad (..))

-- Task 1

data Expr
  = Pow Expr Expr
  | Div Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Add Expr Expr
  | Const Int
  deriving (Show)

data ArithmeticError = DivByZero | NegExp deriving (Eq, Show)

eval :: Expr -> Either ArithmeticError Int
eval (Const c) = Right c
eval (Add l r) = (+) <$> eval l <*> eval r
eval (Sub l r) = (-) <$> eval l <*> eval r
eval (Mul l r) = (*) <$> eval l <*> eval r
eval (Div l r) = case eval r of
                   Left  e -> Left e
                   Right c -> if (c == 0) 
                              then Left DivByZero
                              else (`div` c) <$> eval l
eval (Pow l r) = case eval r of
                   Left  e -> Left e
                   Right c -> if (c < 0)
                              then Left NegExp
                              else (^ c) <$> eval l

-- Task 2

type SmaS = CMS.State ([Double], Double)

movingImpl :: Natural -> Natural -> [Double] -> SmaS [Double]
movingImpl _ _ []      = CMS.return []
movingImpl d 0 (h : t) = do
  (lasts, lastRes) <- CMS.get
  case lasts of
    []      -> error "Internal error."
    lh : lt -> do let res = lastRes - lh + h
                  CMS.put (lt, res)
                  tres <- movingImpl d 0 t
                  CMS.return (res / (fromIntegral d) : tres)
movingImpl d n (h : t) = do
  (lasts, lastRes) <- CMS.get
  let res = lastRes + h
  CMS.put (lasts, res)
  tres <- movingImpl d (n - 1) t
  CMS.return (res / (fromIntegral (d - n + 1)) : tres)

moving :: Natural -> [Double] -> Either String [Double]
moving 0 _ = Left "Zero period."
moving n l = Right $ CMS.evalState (movingImpl n n l) (l, 0)

-- Task 3

class MonadFish m where
  returnFish :: a -> m a
  (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
  returnJoin :: a -> m a
  join       :: m (m a) -> m a

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a

instance MonadFish m => MonadJoin m where
  returnJoin = returnFish

  join = (id >=> id)

instance MonadFish m => Monad m where
  return = returnFish

  a >>= b = (id >=> b) a

instance Monad m => MonadFish m where
  returnFish = return

  (>=>) a b c = a c >>= b

-- To be continued
-- See Block2_1.hs
