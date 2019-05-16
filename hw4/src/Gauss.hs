{-# LANGUAGE BangPatterns #-}

module Gauss
  ( gauss
  , verifySolution
  ) where

import Control.Loop (numLoop, numLoopState)
import Control.Monad (forM_)
import Control.Monad.Loops (whileM_)
import Control.Monad.ST.Strict (runST, ST)
import Data.Array.ST (newArray, STUArray, STArray)
import qualified Data.Array.Base as A (unsafeNewArray_, unsafeRead, unsafeWrite)
import Data.Bits (shiftL, testBit, xor, (.|.))
import Data.List (foldl')
import Data.STRef (modifySTRef', newSTRef, readSTRef)

import Util (parListSplitFold)

toBits_ :: Int -> [[Bool]] -> ST s ((STArray s) Int Integer)
{-# INLINE toBits_ #-}
toBits_ !l sys = do
  !arr <- A.unsafeNewArray_ (0, l - 1)
  i <- newSTRef 0
  forM_ sys $ \eq -> do
    bitSet <- newSTRef 0
    forM_ eq $ \a -> do
      modifySTRef' bitSet (`shiftL` 1)
      if a then modifySTRef' bitSet (.|. 1) else return ()
    i' <- readSTRef i
    bitSet' <- readSTRef bitSet
    A.unsafeWrite arr i' bitSet'
    modifySTRef' i (+ 1)
  return arr

gauss :: [[Bool]] -> [Bool] -> Maybe [Bool]
{-# INLINE gauss #-}
gauss !(e1 : es) !vec =
  let sys' = zipWith (\eq el -> el : eq) (e1 : es) vec
      !n   = length (e1 : es)
      !m   = length e1 in
  runST $ do -- //Mmm... delicious! Looks like C++. AVE CROSSES! DEUS VULT!!!
    placement <- (newArray :: (Int, Int) -> Int -> ST s ((STUArray s) Int Int)) (0, m - 1) (-1)
    !bitSys <- toBits_ n sys'
    col <- newSTRef (m - 1)
    row <- newSTRef 0
    let cond = do
          col' <- readSTRef col
          row' <- readSTRef row
          return $ col' >= 0 && row' < n
    whileM_ cond $ do
      col' <- readSTRef col
      row' <- readSTRef row
      i    <- newSTRef  row'
      let innerCond = do
            i' <- readSTRef i
            if i' < n
            then do
              ith <- A.unsafeRead bitSys i'
              return $ not (testBit ith col')
            else return False
      whileM_ innerCond $ modifySTRef' i (+ 1)
      i' <- readSTRef i
      if i' < n
      then do
        tmp1 <- A.unsafeRead bitSys i'
        tmp2 <- A.unsafeRead bitSys row'
        A.unsafeWrite bitSys i'   tmp2
        A.unsafeWrite bitSys row' tmp1
      else return ()
      rowth <- A.unsafeRead bitSys row'
      if testBit rowth col'
      then do
        A.unsafeWrite placement col' row'
        numLoop (row' + 1) (n - 1) $ \k -> do
          kth <- A.unsafeRead bitSys k
          if testBit kth col'
          then A.unsafeWrite bitSys k (kth `xor` rowth)
          else return ()
        modifySTRef' row (+ 1)
      else return ()
      modifySTRef' col (+ (-1))
    let checkCond = do
          row' <- readSTRef row
          if row' < n
          then do
            rowth <- A.unsafeRead bitSys row'
            return $ not (testBit rowth m)
          else return False
    whileM_ checkCond $ modifySTRef' row (+ 1)
    row' <- readSTRef row
    if row' < n
    then return Nothing
    else Just <$> numLoopState 0 (m - 1) [] (\l i -> do
      p <- A.unsafeRead placement i
      if p < 0
      then return $ False : l
      else do
        pth <- A.unsafeRead bitSys p
        numLoop 0 (p - 1) $ \k -> do
          kth <- A.unsafeRead bitSys k
          if testBit kth i
          then A.unsafeWrite bitSys k (kth `xor` pth)
          else return ()
        return $ testBit pth m : l)
gauss _ _ = error "gauss: empty system"

elemsPerSpark_ :: Int
elemsPerSpark_ = 1048576

verifySolution :: [[Bool]] -> [Bool] -> [Bool] -> Bool
{-# INLINE verifySolution #-}
verifySolution !sys !vec !xs =
  let !n = length xs
      !m = length vec in
  if n * m < 2 * elemsPerSpark_
  then mapF sys'
  else parListSplitFold (elemsPerSpark_ `div` length xs) mapF reduceF sys'
  where
    sys'            = zip sys vec
    reduceF sysPart = foldl' (\ans res -> res && ans) True sysPart
    mapF    sysPart = foldl' eqFold True sysPart
      where
        eqFold res (eq, ans) = res && (not $
          foldl' (\ans' a -> ans' `xor` a) ans (zipWith (\a x -> a && x) eq xs))
