{-# LANGUAGE BangPatterns, DeriveGeneric, DeriveAnyClass #-}

module Geometry
  ( area
  , crossProduct
  , distance
  , minus
  , perimeter
  , plus
  , Point (..)
  , scalarProduct
  ) where

import Control.DeepSeq (force, NFData)
import Data.List (foldl')
import GHC.Generics (Generic)

data Point
  = Point {
    pointX :: !Int
  , pointY :: !Int
  } deriving (Generic, Eq, NFData, Show)

plus :: Point -> Point -> Point
{-# INLINE plus #-}
plus !a !b = Point (pointX a + pointX b) (pointY a + pointY b)

minus :: Point -> Point -> Point
{-# INLINE minus #-}
minus !a !b = Point (pointX a - pointX b) (pointY a - pointY b)

scalarProduct :: Point -> Point -> Int
{-# INLINE scalarProduct #-}
scalarProduct !a !b = pointX a * pointX b + pointY a * pointY b

crossProduct :: Point -> Point -> Int
{-# INLINE crossProduct #-}
crossProduct !a !b = pointX a * pointY b - pointY a * pointX b

distance :: Point -> Point -> Double
{-# INLINE distance #-}
distance !a !b = sqrt . fromIntegral $
  (pointX a - pointX b) ^ (2 :: Int) + (pointY a - pointY b) ^ (2 :: Int)

perimeter :: [Point] -> Double
{-# INLINE perimeter #-}
perimeter !(p1 : p2 : p3 : ps) =
  let (res, lp) = foldl'
        (\(r, prevP) p -> (force $ r + distance prevP p, p)) (0, p1) (p2 : p3 : ps)
  in force $ res + distance p1 lp
perimeter _ = error "perimeter: not a polygon"

area :: [Point] -> Double
{-# INLINE area #-}
area !(p1 : p2 : p3 : ps) =
  let (res, lp) = foldl'
        (\(r, prevP) p -> (force $ r + crossProduct prevP p, p)) (0, p1) (p2 : p3 : ps)
  in force $ (fromIntegral $ res + crossProduct lp p1) / 2
area _ = error "area: not a polygon"
