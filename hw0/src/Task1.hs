{-# LANGUAGE TypeOperators #-}

module Task1
  ( associator
  , distributivity
  , eitherAssoc
  ) where

distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a) = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (eitherAssocL, eitherAssocR)

eitherAssocL :: Either a (Either b c) -> Either (Either a b) c
eitherAssocL (Left a) = Left $ Left a
eitherAssocL (Right (Left b)) = Left $ Right b
eitherAssocL (Right (Right c)) = Right c

eitherAssocR :: Either (Either a b) c -> Either a (Either b c)
eitherAssocR (Left (Left a)) = Left a
eitherAssocR (Left (Right b)) = Right $ Left b
eitherAssocR (Right c) = Right $ Right c
