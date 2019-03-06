module Task2
  ( doubleNeg
  , doubleNegElim
  , excludedNeg
  , pierce
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg = \a -> (\not_a -> not_a a)

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = \not_a_or_not_a -> not_a_or_not_a (Right (\a -> not_a_or_not_a (Left a)))

-- impossible to get
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- impossible to get
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = \not_not_not_a -> \a -> not_not_not_a (doubleNeg a)
