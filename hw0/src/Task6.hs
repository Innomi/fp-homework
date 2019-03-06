module Task6
  ( f
  , foo
  , g
  ) where

import Data.Maybe (mapMaybe)
import Task1 (distributivity)

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True -> Just $ exp pi
    False -> Nothing

-- WHNF = (Left e, Left e)
-- where e is not calculated concat of strings
f :: (Either [Char] b, Either [Char] c)
f = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- WHNF = False
-- null will calculated as False because mapMaybe will produce array of Just
g :: Bool
g = null $ mapMaybe foo "pole chudes ochen' chudesno"
