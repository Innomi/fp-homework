module Bench.Geometry
  ( perimeterBench
  ) where

import Criterion.Main (bench, Benchmark, bgroup, env, nf)

import Geometry (area, crossProduct, distance, perimeter, Point (..))

naivePerimeter :: [Point] -> Double
naivePerimeter (p1 : p2 : p3 : ps) =
  let (res, lp) = foldl
        (\(r, prevP) p -> (r + distance prevP p, p)) (0, p1) (p2 : p3 : ps)
  in res + distance p1 lp
naivePerimeter _ = error "naivePerimeter: not a polygon"

naiveArea :: [Point] -> Double
naiveArea (p1 : p2 : p3 : ps) =
  let (res, _) = foldl
        (\(r, prevP) p -> (r + crossProduct prevP p, p)) (0, p1) (p2 : p3 : ps)
  in (fromIntegral res) / 2
naiveArea _ = error "naiveArea: not a polygon"

setupEnv :: Int -> IO [Point]
setupEnv n = return $ (Point 123 123) : map (\x -> Point x 0) [2..n]

genBench :: Int -> String -> ([Point] -> Double) -> Benchmark
genBench n s f = env (setupEnv n) $ \ ~ps -> bench s $ nf f ps

-- naivePerimeter and naiveArea can kayo your memory with 1e7 points
perimeterBench :: Benchmark
perimeterBench = bgroup "Geometry" [
    bgroup "perimeter" [ genBench 100      "1e2" perimeter
                       , genBench 100000   "1e5" perimeter
                       , genBench 10000000 "1e7" perimeter
                       ]
  , bgroup "naivePerimeter" [ genBench 100     "1e2" naivePerimeter
                            , genBench 100000  "1e5" naivePerimeter
                            , genBench 1000000 "1e6" naivePerimeter
                            ]
  , bgroup "area" [ genBench 100      "1e2" area
                  , genBench 100000   "1e5" area
                  , genBench 10000000 "1e7" area
                  ]
  , bgroup "naiveArea" [ genBench 100     "1e2" naiveArea
                       , genBench 100000  "1e5" naiveArea
                       , genBench 1000000 "1e6" naiveArea
                       ]
  ]
