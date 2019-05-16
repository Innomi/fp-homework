module ShGrammar
  ( Assignment (..)
  , Clause (..)
  , Command (..)
  , CompleteCommand (..)
  , Dollar (..)
  , DoubleQuoted (..)
  , DoubleQuotedPartPart (..)
  , Name (..)
  , Pipeline (..)
  , Word (..)
  , WordPart (..)
  ) where

import qualified Data.Text as T (Text)
import Prelude hiding (Word)

newtype CompleteCommand
  = CompleteCommand [Pipeline]
  deriving (Show)

newtype Pipeline
  = Pipeline [Command]
  deriving (Show)

data Command
  = SimpleCommand [Assignment] [Word]
  | CompoundCommand Clause
  deriving (Show)

data Clause
  = If [CompleteCommand]
  | While CompleteCommand CompleteCommand
  deriving (Show)

data Assignment
  = Assignment Name Word
  deriving (Show)

newtype Name 
  = Name T.Text
  deriving (Eq, Ord, Show)

newtype Word
  = Word [WordPart]
  deriving (Show)

data WordPart
  = TextPart T.Text
  | DollarPart Dollar
  | DoubleQuotedPart DoubleQuoted
  deriving (Show)

newtype DoubleQuoted
  = DoubleQuoted [DoubleQuotedPartPart]
  deriving (Show)

data DoubleQuotedPartPart
  = TextDoubleQuotedPart T.Text
  | DollarDoubleQuotedPart Dollar
  deriving (Show)

data Dollar
  = Var Name
  | Inline CompleteCommand
  deriving (Show)
