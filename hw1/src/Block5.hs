module Block5
  ( Builder (..)
  , eitherConcat
  , Endo (..)
  , fromString
  , maybeConcat
  , Name (..)
  , NonEmpty (..)
  , ThisOrThat (..)
  , toString
  ) where

-- Task 1 + hard version

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat []             = []
maybeConcat (Nothing : t)  = maybeConcat t
maybeConcat ((Just l) : t) = l ++ (maybeConcat t)

eitherConcat :: (Monoid m1, Monoid m2) => [Either m1 m2] -> (m1, m2)
eitherConcat []              = (mempty, mempty)
eitherConcat ((Right h) : t) = let (l, r) = eitherConcat t in (l, h <> r)
eitherConcat ((Left h) : t)  = let (l, r) = eitherConcat t in (h <> l, r)

-- Task 2 + hard version

data NonEmpty a = a :| [a] deriving (Show)

instance Semigroup (NonEmpty a) where
  (hl :| tl) <> (hr :| tr) = hl :| (tl ++ (hr : tr))

data ThisOrThat a b = This a | That b | Both a b deriving (Show)

instance Semigroup (ThisOrThat a b) where
  This a   <> _ = This a
  That b   <> _ = That b
  Both a _ <> _ = This a

data Name = Empty | Name String deriving (Show)

instance Semigroup Name where
  l        <> Empty    = l
  Empty    <> r        = r
  (Name l) <> (Name r) = Name (l ++ ('.' : r))

instance Monoid Name where
  mempty = Empty

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  (Endo l) <> (Endo r) = Endo (l . r)

instance Monoid (Endo a) where
  mempty = Endo id

-- Task 3

data Builder = One Char | Many [Builder] deriving (Show)

instance Semigroup Builder where
  One cl  <> One cr  = Many ((One cl) : [One cr])
  One cl  <> Many lr = case lr of (_ : _) -> Many ((One cl) : lr)
                                  []      -> One cl
  Many ll <> One cr  = case ll of (_ : _) -> Many [Many ll, One cr]
                                  []      -> One cr
  Many ll <> Many lr = Many [Many ll, Many lr]

instance Monoid Builder where
  mempty = Many []

fromString :: String -> Builder
fromString []      = mempty
fromString (h : t) = (One h) <> fromString t

toString :: Builder -> String
toString (One c)        = [c]
toString (Many [])      = []
toString (Many (h : t)) = (toString h) ++ (toString (Many t))
