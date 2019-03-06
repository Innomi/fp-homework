module Task4
  ( factorial
  , fibonacci
  , iterateElement
  , mapFix
  ) where

import Data.Function (fix)

iterateElement :: a -> [a]
iterateElement = fix (\f x -> f x)

fibonacci :: Integer -> Integer
fibonacci = fix (\f x -> if (x < 0)
                         then error "negative index of fibonacci num"
                         else if (x < 2)
                              then x
                              else (f (x - 1) + f (x - 2))
                )

factorial :: Integer -> Integer
factorial = fix (\f x -> if (x < 0) 
                         then error "factorial of negative Integer"
                         else if (x == 0)
                              then 1
                              else ((f (x - 1)) * x)
                )

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix (\f g l -> case l of
                          [] -> []
                          x : xs -> (g x) : (f g xs)
             )
