module Block2
  ( delListElIndex
  , mergeSort
  , randomIntList
  ) where

import Data.List (groupBy)
import System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

-- Task 1

delListElIndex :: Integral a => a -> [b] -> (Either String b, [b])
delListElIndex i (h : t) | i == 0    = (Right h, t)
                         | i >  0    = let (el, l) = delListElIndex (i - 1) t in (el, h : l)
                         | otherwise = (Left "negative index", h : t)
delListElIndex _ []      = (Left "out of bound", [])

-- Task 2

merge :: Ord a => [a] -> [a] -> [a]
merge (hl : tl) (hr : tr) | hl <= hr  = hl : merge tl (hr : tr)
                          | otherwise = hr : merge (hl : tl) tr
merge []        r         = r
merge l         []        = l

mergeSortImpl2 :: Ord a => [[a]] -> [[a]]
mergeSortImpl2 (h1 : h2 : t) = merge h1 h2 : mergeSortImpl2 t
mergeSortImpl2 (h : [])      = [h]
mergeSortImpl2 []            = []

mergeSortImpl1 :: Ord a => [[a]] -> [a]
mergeSortImpl1 (h : []) = h
mergeSortImpl1 (h : t)  = mergeSortImpl1 (mergeSortImpl2 (h : t))
mergeSortImpl1 []       = []

mergeSort :: Ord a => [a] -> [a]
mergeSort l = mergeSortImpl1 $ groupBy (\_ _ -> False) l
