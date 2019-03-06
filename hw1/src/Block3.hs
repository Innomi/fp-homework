module Block3
  ( afterDays
  , BinaryTree (..)
  , buildCastle
  , buildCultureBuilding
  , buildHouse
  , buildWall
  , daysToParty
  , delete
  , empty
  , evenNat
  , find
  , Block3.fromList
  , fromSEnum
  , insert
  , isWeekend
  , moveLordInDaCastle
  , nextDay
  , oddNat
  , spred
  , ssucc
  , toSEnum
  , treeSize
  ) where

import Data.List.NonEmpty as NE (NonEmpty (..), (<|), nonEmpty, fromList, head, tail)

-- Task 1

data WeekDay
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Satureday
  | Sunday
  deriving (Show)

class SEnum a where
  toSEnum   :: Integral i => i -> a
  fromSEnum :: a -> Integer
  ssucc     :: a -> a
  spred     :: a -> a

  ssucc = toSEnum . (+ 1) . fromSEnum
  spred = toSEnum . (subtract 1) . fromSEnum

instance SEnum WeekDay where
  toSEnum a = case (mod (toInteger a) 7) of 0 -> Monday
                                            1 -> Tuesday
                                            2 -> Wednesday
                                            3 -> Thursday
                                            4 -> Friday
                                            5 -> Satureday
                                            _ -> Sunday

  fromSEnum a = case a of Monday    -> 0
                          Tuesday   -> 1
                          Wednesday -> 2
                          Thursday  -> 3
                          Friday    -> 4
                          Satureday -> 5
                          Sunday    -> 6

nextDay :: WeekDay -> WeekDay
nextDay = ssucc

afterDays :: Integral i => WeekDay -> i -> WeekDay
afterDays d a | (toInteger a) <= 0    = d
              | otherwise = toSEnum . ((toInteger a) +) . fromSEnum $ d

isWeekend :: WeekDay -> Bool
isWeekend Sunday    = True
isWeekend Satureday = True
isWeekend _         = False

daysToParty :: WeekDay -> String
daysToParty d = show (mod ((fromSEnum Friday) + 7 - (fromSEnum d)) 7)

-- Task 2

data Occupants
  = One
  | Two
  | Three
  | Four
  deriving (Show)

instance Enum Occupants where
  toEnum i = case i of 0 -> One
                       1 -> Two
                       2 -> Three
                       3 -> Four
                       _ -> error "illegal enum index"

  fromEnum o = case o of One   -> 0
                         Two   -> 1
                         Three -> 2
                         Four  -> 3

newtype House  = House Occupants deriving (Show)
newtype Castle = Castle (Maybe Lord) deriving (Show)

data Church  = Church deriving (Show)
data Library = Library deriving (Show)
data Lord    = Lord deriving (Show)
data Wall    = Wall deriving (Show)

data City = City
  { castleWall      :: Maybe (Castle, Maybe Wall)
  , cultureBuilding :: Maybe (Either Church Library)
  , houses          :: NonEmpty House
  } deriving (Show)

buildCastle :: City -> (Bool, City)
buildCastle c = case castleWall c of Nothing -> (True, c { castleWall = 
                                                  Just (Castle Nothing, Nothing) })
                                     Just _  -> (False, c)

buildCultureBuilding :: City -> Either Church Library -> (Bool, City)
buildCultureBuilding c b = case cultureBuilding c of Nothing -> 
                                                       (True, c { cultureBuilding = Just b })
                                                     Just _ -> (False, c)

buildHouse :: City -> Occupants -> City
buildHouse c o = c { houses = House o <| houses c }

moveLordInDaCastle :: City -> Lord -> Either String City
moveLordInDaCastle c l = case castleWall c of
                           Nothing                      -> 
                             Left "no castle in da city"
                           Just (Castle (Just Lord), _) ->
                             Left "castle is occupied by another lord"
                           Just (Castle Nothing, w)     ->
                             Right c { castleWall = Just (Castle (Just l), w) }

enoughCitizens :: NonEmpty House -> Int -> Bool
enoughCitizens ((House o) :| [])       n = (n + (fromEnum o) + 1) > 9
enoughCitizens ((House o) :| (h2 : t)) n | (n + (fromEnum o) + 1) > 9 = True
                                         | otherwise                  = enoughCitizens 
                                                                        (h2 :| t) 
                                                                        (n + (fromEnum o) + 1)

buildWall :: City -> Either String City
buildWall c = case castleWall c of 
                Nothing                      -> Left "no castle in da city"
                Just (Castle _, Just Wall)   -> Left "the wall is already built"
                Just (Castle Nothing, _)     -> Left "no lord in da city"
                Just (Castle (Just Lord), _) -> 
                  if (enoughCitizens (houses c) 0)
                  then Right (c { castleWall = Just (Castle (Just Lord), Just Wall) })
                  else Left "not enough slaves"

-- Task 3 + hard version

data Nat = Z | S Nat deriving (Show)

instance Num Nat where
  a + Z     = a
  a + (S b) = (+) (S a) b

  _ * Z     = Z
  a * (S b) = a + (a * b)

  a     - Z     = a
  (S a) - (S b) = a - b
  Z     - (S _) = error "negative result" 

  abs a = a

  signum Z = Z
  signum _ = S Z

  fromInteger a | a <  0    = error "nat cannot be negative"
                | a == 0    = Z
                | otherwise = S (fromInteger (a - 1))

instance Eq Nat where
  (S a) == (S b) = a == b
  Z     == Z     = True
  _     == _     = False

instance Ord Nat where
  compare Z     Z     = EQ
  compare _     Z     = GT
  compare Z     _     = LT
  compare (S a) (S b) = compare a b

instance Real Nat where
  toRational a = toRational (toInteger a)

instance Enum Nat where
  toEnum i = fromInteger (toInteger i)

  fromEnum Z     = 0
  fromEnum (S a) = 1 + (fromEnum a)

instance Integral Nat where
  toInteger Z     = 0
  toInteger (S a) = 1 + (toInteger a)

  div _ Z = error "division by zero"
  div a b | b <= a    = S (div (a - b) b)
          | otherwise = Z

  mod _ Z = error "division by zero"
  mod a b | b <= a    = mod (a - b) b
          | otherwise = a

  quotRem a b = (div a b, mod a b)

evenNat :: Nat -> Bool
evenNat a = mod a (S (S Z)) == Z

oddNat :: Nat -> Bool
oddNat a = not (evenNat a)

-- Task 4

data BinaryTree e
  = Leaf
  | Node (BinaryTree e) (NonEmpty e) (BinaryTree e)
  deriving (Show)

empty :: BinaryTree e -> Bool
empty Leaf = True
empty _    = False

treeSize :: BinaryTree e -> Int
treeSize Leaf         = 0
treeSize (Node l d r) = (length d) + (treeSize l) + (treeSize r)

find :: Ord e => BinaryTree e -> e -> Maybe (BinaryTree e)
find Leaf         _ = Nothing
find (Node l d r) e | e < NE.head d = find l e
                    | e > NE.head d = find r e
                    | otherwise  = Just (Node l d r)

insert :: Ord e => BinaryTree e -> e -> BinaryTree e
insert Leaf         e = Node Leaf (NE.fromList [e]) Leaf
insert (Node l d r) e | e < NE.head d = Node (insert l e) d r
                      | e > NE.head d = Node l d (insert r e)
                      | otherwise  = Node l (e <| d) r

fromList :: Ord e => [e] -> BinaryTree e
fromList []      = Leaf
fromList (h : t) = insert (Block3.fromList t) h

delete :: Ord e => BinaryTree e -> e -> BinaryTree e
delete Leaf         _ = Leaf
delete (Node l d r) e | e < NE.head d = Node (delete l e) d r
                      | e > NE.head d = Node l d (delete r e)
                      | otherwise     = case nonEmpty (NE.tail d) of
                                          Nothing -> Leaf
                                          Just t  -> Node l t r
