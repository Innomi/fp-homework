module Block1
  ( NonEmpty (..)
  , stringSum
  , Tree (..)
  ) where

import Data.Foldable (foldl')
import Text.Read (readMaybe)

-- Task 1

stringSum :: String -> Maybe Int
stringSum s = (foldl' (\a b -> a + b) 0) <$> (traverse readMaybe $ words s)

-- Task 2

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Show)

instance Functor Tree where
  fmap f (Leaf d)     = Leaf (f d)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
  pure = Leaf

  Leaf f       <*> t          = fmap f t
  Branch fl fr <*> Leaf d     = Leaf ($ d) <*> Branch fl fr
  Branch fl fr <*> Branch l r = Branch (fl <*> l) (fr <*> r)

instance Foldable Tree where
  foldMap f (Leaf d)     = f d
  foldMap f (Branch l r) = foldMap f l <> foldMap f r

instance Traversable Tree where
  traverse f (Leaf d)     = Leaf <$> f d
  traverse f (Branch l r) = Branch <$> traverse f l <*> traverse f r

-- Task 3

data NonEmpty a = a :| [a] deriving (Show)

instance Functor NonEmpty where
  fmap f (h :| t) = (f h) :| (fmap f t)

instance Applicative NonEmpty where
  pure d = d :| []

  hf :| tf <*> hd :| td = let ds = hd : td in
                          (hf hd) :| ((fmap hf td) ++ (tf <*> ds))

instance Monad NonEmpty where
  (h :| [])        >>= f = f h
  (h1 :| (h2 : t)) >>= f = let (nh2 :| nt) = (h2 :| t) >>= f in
                           let (nh1 :| _) = f h1 in
                           nh1 :| (nh2 : nt)

instance Foldable NonEmpty where
  foldMap f (h :| t) = f h <> foldMap f t

instance Traversable NonEmpty where
  traverse f (h :| t) = (:|) <$> f h <*> traverse f t
