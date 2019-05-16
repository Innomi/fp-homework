module Test.CHT
  ( chtTests
  ) where

import CHT (ConcurrentHashTable (..), getCHT, newCHT, putCHT, sizeCHT)

import Control.Concurrent.Async (forConcurrently_)
import Control.Monad (forM_)
import Data.Hashable (Hashable)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Random (newStdGen, randoms)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, it, shouldReturn, testSpec)

getRandomPairs :: Int -> IO [(Int, Int)]
getRandomPairs s = do
  g <- newStdGen
  let keys = take s (randoms g :: [Int])
      vals = take s (randoms g :: [Int])
      pairs = zip keys vals
  return pairs

checkCHT :: (Eq k, Eq v, Hashable k) => ConcurrentHashTable k v -> [(k, v)] -> IO Bool
checkCHT cht pairs = do
  ok <- newIORef True
  forM_ pairs $ \(k, v) -> do
    mv' <- getCHT k cht
    case mv' of Nothing -> writeIORef ok False
                Just v' -> if (v' /= v) then writeIORef ok False else return ()
  ok' <- readIORef ok
  return ok'

oneThreadPuts :: IO Bool
oneThreadPuts = do
  pairs <- getRandomPairs 10000
  cht <- newCHT
  forM_ pairs $ \(k, v) -> do
    putCHT k v cht
  checkCHT cht pairs

putWorker :: (Eq k, Hashable k) => [(k, v)] -> ConcurrentHashTable k v -> IO ()
putWorker pairs table = do
  forM_ pairs $ \(k, v) -> do
    putCHT k v table

concurrentPuts :: IO Bool
concurrentPuts = do
  pairs1 <- getRandomPairs 100000
  pairs2 <- getRandomPairs 100000
  pairs3 <- getRandomPairs 100000
  pairs4 <- getRandomPairs 100000
  let pairs = pairs1 ++ pairs2 ++ pairs3 ++ pairs4
      jobs  = [pairs1, pairs2, pairs3, pairs4]
  cht <- newCHT
  forConcurrently_ jobs $ (flip putWorker) cht
  finalSize <- sizeCHT cht
  checkCHT cht pairs

chtHspecTest :: Spec
chtHspecTest = do
  it "one thread put" $ do
    oneThreadPuts `shouldReturn` True
  it "concurrent puts" $ do
    concurrentPuts `shouldReturn` True

chtTests :: IO TestTree
chtTests = testSpec "CHT tests" chtHspecTest
