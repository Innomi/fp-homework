{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Block4
  ( joinWith
  , NonEmpty (..)
  , Pair (..)
  , splitOn
  ) where

import Block3 (BinaryTree (..))

-- Task 1 + hard version

data Pair a = Pair a a deriving (Show)

instance Foldable Pair where
  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair l r) = (f l) <> (f r)

  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f b (Pair l r) = f l (f r b)

data NonEmpty a = a :| [a] deriving (Show)

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (h :| [])        = f h
  foldMap f (h1 :| (h2 : t)) = (f h1) <> (foldMap f (h2 :| t))

  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f b (h1 :| (h2 : t)) = f h1 (foldr f b (h2 :| t))
  foldr f b (h :| [])        = f h b

instance Foldable BinaryTree where
  foldMap :: Monoid m => (a -> m) -> BinaryTree a -> m
  foldMap _ Leaf         = mempty
  foldMap f (Node l d r) = (foldMap f l) <> (foldMap f d) <> (foldMap f r)

  foldr :: (a -> b -> b) -> b -> BinaryTree a -> b
  foldr _ b Leaf         = b
  foldr f b (Node l d r) = foldr f (foldr f (foldr f b r) d) l

-- Task 2 + hard version

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn e = foldr (\a ll -> case ll of (h :| t) -> if (a == e) 
                                                   then [] :| (h : t)
                                                   else ((a : h) :| t)
                  ) 
                  ([] :| [])

joinWith :: Eq a => a -> NonEmpty [a] -> [a]
joinWith e = tail . (foldr (\a l -> (e : a) ++ l) [])
