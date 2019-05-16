module Bench.Multiply
  ( multiplyBench
  ) where

import Criterion.Main (bench, Benchmark, bgroup, env, nf)

import qualified Data.Matrix as M (fromLists, toLists)

import Multiply (multiply)

naiveMultiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
naiveMultiply a b = Just $ M.toLists ((M.fromLists a) * (M.fromLists b))

setupEnv :: Int -> IO ([[Int]], [[Int]])
setupEnv n = do
  let a = replicate n [1..n]
      b = replicate n [1..n]
  return (a, b)

genBench :: Int -> String -> ([[Int]] -> [[Int]] -> Maybe [[Int]]) -> Benchmark
genBench n s f = env (setupEnv n) $ \ ~(a, b) -> bench s $ nf (uncurry f) (a, b)

-- It is bad idea to try to multiply 1000x1000 and 2000x2000 matrices using
-- Data.Matrix.(*) trust me.
multiplyBench :: Benchmark
multiplyBench = bgroup "Multiply" [
    bgroup "multiply" [ genBench 10   "10x10"     multiply
                      , genBench 100  "100x100"   multiply
                      , genBench 500  "500x500"   multiply
                      , genBench 1000 "1000x1000" multiply
                      , genBench 2000 "2000x2000" multiply
                      ]
  , bgroup "naiveMultiply" [ genBench 10  "10x10"   naiveMultiply
                           , genBench 100 "100x100" naiveMultiply
                           , genBench 500 "500x500" naiveMultiply
                           ]
  ]
