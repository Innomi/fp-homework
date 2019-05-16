{-# LANGUAGE BangPatterns #-}

module Multiply
  ( multiply
  ) where

import Control.Loop (numLoopFold)
import Control.Monad (forM_)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (runSTUArray)
import qualified Data.Array.Base as A (unsafeAt, unsafeNewArray_, unsafeWrite)
import Data.STRef (modifySTRef', newSTRef, readSTRef)

import Util (parMapChunk)

data Matrix
  = Matrix {
    rows    :: !Int
  , cols    :: !Int
  , dataArr :: !(UArray Int Int)
  } deriving (Show)

-- Here is no need in concurrency since it works extremely fast in compare to
-- another stuff. 
matrix_ :: [[Int]] -> Matrix
{-# INLINE matrix_ #-}
matrix_ []      = error "matrix: empty list"
matrix_ (h : t) =
  let !r = length t + 1
      !c = length h in
  Matrix r c $ runSTUArray $ do
    a <- A.unsafeNewArray_ (0, r * c - 1)
    i <- newSTRef 0
    forM_ (h : t) $ \l -> do -- Is it Java? (here should be meme with butterfly)
      j <- newSTRef 0
      forM_ l $ \e -> do
        i' <- readSTRef i
        j' <- readSTRef j
        A.unsafeWrite a (i' * c + j') e
        modifySTRef' j (+ 1)
      modifySTRef' i (+ 1)
    return a

transposedMatrix_ :: [[Int]] -> Matrix
{-# INLINE transposedMatrix_ #-}
transposedMatrix_ []      = error "traversedMatrix: empty list"
transposedMatrix_ (h : t) =
  let !r = length h
      !c = length t + 1 in
  Matrix r c $ runSTUArray $ do
    a <- A.unsafeNewArray_ (0, r * c - 1)
    i <- newSTRef 0
    forM_ (h : t) $ \l -> do
      j <- newSTRef 0
      forM_ l $ \e -> do
        i' <- readSTRef i
        j' <- readSTRef j
        A.unsafeWrite a (j' * c + i') e
        modifySTRef' j (+ 1)
      modifySTRef' i (+ 1)
    return a

calcEl_ :: Int -> Int -> Int -> UArray Int Int -> UArray Int Int -> Int
{-# INLINE calcEl_ #-}
calcEl_ !i !j !l !a !b =
  let !begA = i * l
      !begB = j * l in
  numLoopFold 0 (l - 1) 0 $ \acc k ->
    acc + ((A.unsafeAt a (begA + k)) * (A.unsafeAt b (begB + k)))

elemsPerSpark_ :: Int
elemsPerSpark_ = 1048576

-- Note : evaluating a' and b' concurrently makes a lot of GC'd or fizzled sparks.
multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
{-# INLINE multiply #-}
multiply a b =
  let !a' = matrix_ a
      !b' = transposedMatrix_ b
      !dataA = dataArr a'
      !dataB = dataArr b'
      !l = cols a'
      !r = rows a'
      !c = rows b' in
  if l /= cols b'
  then Nothing
  else Just $
    if r * c * l < 2 * elemsPerSpark_
    then
      numLoopFold 1 r [] $ \matr i ->
        (flip (:)) matr $ numLoopFold 1 c [] $ \row j ->
          (flip (:)) row $ calcEl_ (r - i) (c - j) l dataA dataB
    else
      (flip $ parMapChunk $ elemsPerSpark_ `div` (l * c)) [1..r] $ \i -> 
        numLoopFold 1 c [] $ \row j ->
          (flip (:)) row $ calcEl_ (r - i) (c - j) l dataA dataB
