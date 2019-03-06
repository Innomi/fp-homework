module Task3
  ( composition
  , contraction
  , identity
  , permutation
  , s
  ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

identity :: a -> a
identity = s const const

contraction :: (a -> a -> b) -> a -> b
contraction = s s (const (s const const))

permutation :: (a -> b -> c) -> b -> a -> c
permutation = s ((s (const s) const) (s (const s) const) s) (const const)
