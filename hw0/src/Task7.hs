module Task7
  ( f
  , g
  , h
  ) where

import Data.Either (lefts, rights)

f :: Bool
f = (($) :: (a -> b) -> a -> b)
    (
      (
        (.) 
        :: (b -> c) -> (a -> b) -> a -> c
      )
      (
        null 
        :: Foldable t => t a -> Bool
      ) 
      (
        head 
        :: [a] -> a
      ) 
    :: Foldable t => [t a] -> Bool
    )
    (
      (
        map 
        :: (a -> b) -> [a] -> [b]
      )
      (
        (
          uncurry
          :: (a -> b -> c) -> (a, b) -> c
        )
        (
          id
          :: a -> a
        )
      )
      (
        [((++) "Dorian ", " Grey")]
        :: [([Char] -> [Char], [Char])]
      )
      :: [[Char]]
    )

g :: [(Integer, Integer)]
g = (
      (\x ->
        ( 
          zip :: [a] -> [b] -> [(a, b)]
        )
        (
          (
            (
              lefts 
              :: [Either a b] -> [a]
            )
            x
          )
        )
        (
          (
            (
            rights
            :: [Either a b] -> [b]
            )
            x
          )
        )
      )
      :: [Either a b] -> [(a, b)]
    )
    (
      [Left (1 + 2), Right (2 ^ 6)]
      :: (Num a, Num b) => [Either a b]
    )

h :: Integer -> Bool
h = let impl = \x y -> (not :: Bool -> Bool) (((||) :: Bool -> Bool -> Bool) x y) in
      let isMod2 = \x -> ((==) :: Integer -> Integer -> Bool) (((mod) :: Integer -> Integer -> Integer) x 2) 0 in
      let isMod4 = \x -> ((==) :: Integer -> Integer -> Bool) ((mod :: Integer -> Integer -> Integer) x 4) 0 in
      \x -> (impl :: Bool -> Bool -> Bool) ((isMod4 :: Integer -> Bool) x) ((isMod2 :: Integer -> Bool) x)
