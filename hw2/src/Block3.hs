module Block3
  ( element
  , eof
  , integerParser
  , integerEofParser
  , listsIntegerParser
  , ok
  , Parser (..)
  , pspEofParser
  , pspParser
  , satisfy
  , stream
  ) where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit, digitToInt)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Numeric.Natural (Natural)

-- Task 1

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap f (Parser parser) = Parser (fmap (\(b, c) -> (f b, c)) . parser)

instance Applicative (Parser st) where
  pure a = Parser $ \s -> Just (a, s)

  Parser pf <*> Parser pa = Parser $ \s -> 
    pf s >>= \(f, t) -> pa t >>= \(a, r) -> Just (f a, r)

instance Monad (Parser st) where
  return = pure

  Parser p >>= f = Parser $ \s -> p s >>= \(a, r) -> runParser (f a) r

instance Alternative (Parser st) where
  empty = Parser $ \_ -> Nothing

  Parser pa <|> Parser pb = Parser $ \s -> pa s <|> pb s

-- Task 2

ok :: Parser st ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser st ()
eof = Parser $ \s -> case s of [] -> Just ((), [])
                               _  -> Nothing

satisfy :: (st -> Bool) -> Parser st st
satisfy p = Parser $ \s -> case s of []    -> Nothing
                                     h : t -> if (p h)
                                              then Just (h, t)
                                              else Nothing

element :: Eq et => et -> Parser et et
element e = satisfy (e ==)

stream :: Eq et => [et] -> Parser et [et]
stream []      = return []
stream (h : t) = (:) <$> (element h) <*> stream t

-- Task 3

pspParser :: Parser Char ()
pspParser = () <$ many (element '(' *> pspParser <* element ')')

pspEofParser :: Parser Char ()
pspEofParser = pspParser *> eof

signParser :: Parser Char Integer
signParser = -1 <$ element '-' <|> 1 <$ element '+' <|> 1 <$ ok

digitParser :: Parser Char Integer
digitParser = toInteger . digitToInt <$> satisfy isDigit

integerParser :: Parser Char Integer
integerParser = ((*) <$> signParser) <*>
                (fmap (foldl' (\n d -> n * 10 + d) 0) $ some digitParser)

integerEofParser :: Parser Char Integer
integerEofParser = integerParser <* eof

-- Task 4

wss :: NonEmpty String
wss = " " :| []

seps :: NonEmpty String
seps = "," :| []

skip :: NonEmpty String -> Parser Char ()
skip (h :| t) = () <$ foldl (\a b -> a <|> stream b) (stream h) t

skipWss :: Parser Char ()
skipWss = () <$ many (skip wss)

sepParser :: Parser Char ()
sepParser = skipWss *> skip seps

wssEof :: Parser Char ()
wssEof = skipWss *> eof

wssIntegerParser :: Parser Char Integer
wssIntegerParser = skipWss *> integerParser

sizeParser :: Parser Char Natural
sizeParser = wssIntegerParser >>= (\n -> if (n < 0) 
                                         then empty 
                                         else return (fromInteger n))

elementsParser :: Natural -> Parser Char [Integer]
elementsParser 0 = pure []
elementsParser n = (:) <$> 
                   (sepParser *> wssIntegerParser) <*>
                   elementsParser (n - 1)

listIntegerParser :: Parser Char [Integer]
listIntegerParser = sizeParser >>= elementsParser

listsIntegerParser :: Parser Char [[Integer]]
listsIntegerParser = (((:) <$> 
                     listIntegerParser <*>
                     many (sepParser *> listIntegerParser)) <|>
                     [] <$ ok) <* wssEof
