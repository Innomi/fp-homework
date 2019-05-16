module Bench.Gauss
  ( gaussBench
  ) where

import Criterion.Main (bench, Benchmark, bgroup, env, nf)

import Control.Loop (numLoopState)
import Data.Maybe (fromJust)
import System.Random (newStdGen, randoms)

import Gauss (gauss, verifySolution)

setupEnv :: Int -> IO ([[Bool]], [Bool])
setupEnv n = do
  let vec    = replicate n True
      matrix = replicate n vec
  return (matrix, vec)

setupRandEnv :: Int -> IO ([[Bool]], [Bool])
setupRandEnv n = do
  gen <- newStdGen
  let vec = take n $ randoms gen
  matrix <- numLoopState 0 (n - 1) [] $ \l _ ->
              return $ (take n $ randoms gen) : l
  return (matrix, vec)

setupEnvAns :: Int -> IO ([[Bool]], [Bool], [Bool])
setupEnvAns n = do
  (matrix, vec) <- setupEnv n
  let ans = fromJust $ gauss matrix vec
  return (matrix, vec, ans)

genBenchGauss :: Int -> String -> (Int -> IO ([[Bool]], [Bool])) -> Benchmark
genBenchGauss n s setter = env (setter n) $ \ ~(matrix, vec) ->
  bench s $ nf (uncurry gauss) (matrix, vec)

genBenchVerifySolution :: Int -> String -> Benchmark
genBenchVerifySolution n s = env (setupEnvAns n) $ \ ~(matrix, vec, ans) ->
  bench s $ nf (uncurry $ uncurry verifySolution) ((matrix, vec), ans)

gaussBench :: Benchmark
gaussBench = bgroup "Gauss" [
    bgroup "gauss" [ genBenchGauss 10   "10x10"         setupEnv
                   , genBenchGauss 100  "100x100"       setupEnv
                   , genBenchGauss 500  "500x500"       setupEnv
                   , genBenchGauss 1000 "1000x1000"     setupEnv
                   , genBenchGauss 1000 "1000x1000Rand" setupRandEnv
                   , genBenchGauss 5000 "5000x5000"     setupEnv
                   , genBenchGauss 5000 "5000x5000Rand" setupRandEnv
                   ]
  , bgroup "verifySolution" [ genBenchVerifySolution 10   "10x10"
                            , genBenchVerifySolution 100  "100x100"
                            , genBenchVerifySolution 500  "500x500"
                            , genBenchVerifySolution 1000 "1000x1000"
                            , genBenchVerifySolution 5000 "5000x5000"
                            ]
  ]
