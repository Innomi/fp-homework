module Block1
  ( contains
  , order3
  , smartReplicate
  , stringSum
  ) where

import Data.List (genericReplicate)

-- Task 1

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (min (min a b) c, min (min (max a b) (max b c)) (max a c), max (max a b) c)

-- Task 2

smartReplicate :: Integral a => [a] -> [a]
smartReplicate (h : t) = genericReplicate h h ++ smartReplicate t
smartReplicate []      = []

-- Task 3

contains :: Eq a => a -> [[a]] -> [[a]]
contains a (h : t) | elem a h  = h : (contains a t)
                   | otherwise = contains a t
contains _ []      = []

-- Task 4

stringSum :: String -> Int
stringSum s = foldl (\a b -> a + (read b)) 0 (words s)
