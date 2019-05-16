module Bench.CHT
  ( chtBench
  ) where

import Criterion.Main (bench, Benchmark, bgroup, nfAppIO)

import Control.Concurrent.Async (async, wait)
import Control.Monad (forM_)
import System.Random (randomIO)

import CHT (ConcurrentHashTable, newCHT, putCHT, getCHT)

worker :: Int -> Int -> Int -> ConcurrentHashTable Int Int -> IO ()
worker opsN putsR maxKey cht = do
  forM_ [1..opsN] $ \_ -> do
    rand <- (randomIO :: IO Int)
    if rand `mod` 100 < putsR
    then do
      k <- (randomIO :: IO Int)
      v <- (randomIO :: IO Int)
      putCHT (k `mod` maxKey) v cht
    else do
      k <- (randomIO :: IO Int)
      () <$ getCHT (k `mod` maxKey) cht

benchSuit :: Int -> Int -> IO ()
benchSuit opsN putsR = do
  cht <- (newCHT :: IO (ConcurrentHashTable Int Int))
  w1 <- async $ worker (opsN `div` 4) putsR opsN cht
  w2 <- async $ worker (opsN `div` 4) putsR opsN cht
  w3 <- async $ worker (opsN `div` 4) putsR opsN cht
  w4 <- async $ worker (opsN `div` 4) putsR opsN cht
  wait w1
  wait w2
  wait w3
  wait w4

genBenchCHT :: Int -> Int -> String -> Benchmark
genBenchCHT opsN putsR s = bench s $ nfAppIO (uncurry benchSuit) (opsN, putsR)

chtBench :: Benchmark
chtBench = bgroup "CHT" [
    bgroup "cht" [ genBenchCHT 100000 80 "1e5 ops : 80% puts | 20% gets"
                 , genBenchCHT 100000 20 "1e5 ops : 20% puts | 80% gets"
                 ]
  ]
