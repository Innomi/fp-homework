module Main
  ( main
  ) where

import Test.Block1 (stringSumTests)
import Test.Block2 (evalTests)
import Test.Block3 (parsersTests)

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = stringSumTests >>= \stringSumUTests ->
       evalTests >>= \evalUTests ->
       parsersTests >>= \parsersUTests ->
       let allTests = testGroup "Hw2 tests" [stringSumUTests, evalUTests, parsersUTests] in
       defaultMain allTests
