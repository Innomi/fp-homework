module ShInterpreter
  ( interpret
  , interpretCommand
  ) where

import Control.Monad.Reader (ask, liftIO, ReaderT)
import qualified Data.Map.Strict as M (insert, lookup, Map)
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Text as T (concat, empty, Text, unwords, words)
import Prelude hiding (Word)

import ShGrammar

data Env = Env
  { varsIORef :: IORef (M.Map Name T.Text)
  }

type InterpreterIO a = ReaderT Env IO a

chgVar :: Name -> T.Text -> InterpreterIO ()
chgVar name val = do
  Env varsRef <- ask
  vars <- liftIO $ readIORef varsRef
  let _ = M.insert name val vars
  liftIO $ writeIORef varsRef vars

trim :: T.Text -> T.Text
trim = T.unwords . T.words

expandDolllar :: Dollar -> InterpreterIO T.Text
expandDolllar (Var name) = do
  Env varsRef <- ask
  vars <- liftIO $ readIORef varsRef
  return (case (M.lookup name vars) of Nothing -> mempty
                                       Just text -> text)
expandDolllar (Inline command) = do
  Env varsRef <- ask
  vars <- liftIO $ readIORef varsRef
  res <- interpretCompleteCommand command
  liftIO $ writeIORef varsRef vars
  return $ res -- TODO

expandDoubleQuotedImpl :: [DoubleQuotedPartPart] -> InterpreterIO [T.Text]
expandDoubleQuotedImpl []      = return []
expandDoubleQuotedImpl (h : t) =
  case h of
    TextDoubleQuotedPart text  -> (text :) <$> (expandDoubleQuotedImpl t)
    DollarDoubleQuotedPart dol -> (:) <$> (expandDolllar dol) <*> (expandDoubleQuotedImpl t)

expandDoubleQuoted :: DoubleQuoted -> InterpreterIO T.Text
expandDoubleQuoted (DoubleQuoted dqParts) = (T.concat) <$> expandDoubleQuotedImpl dqParts

concatWordImpl :: [WordPart] -> InterpreterIO [T.Text]
concatWordImpl []      = return []
concatWordImpl (h : t) =
  case h of 
    (TextPart text)        -> (text :) <$> (concatWordImpl t)
    (DollarPart dol)       -> (:) <$> (expandDolllar dol)      <*> (concatWordImpl t)
    (DoubleQuotedPart dqp) -> (:) <$> (expandDoubleQuoted dqp) <*> (concatWordImpl t)

concatWord :: Word -> InterpreterIO T.Text
concatWord (Word wordParts) = T.concat <$> (concatWordImpl wordParts)

assignVars :: [Assignment] -> InterpreterIO ()
assignVars []      = return ()
assignVars ((Assignment name word) : t) = do
  Env varsRef <- ask
  vars <- liftIO $ readIORef varsRef
  (M.insert name) <$> (concatWord word) <*> (pure vars)
  liftIO $ writeIORef varsRef vars
  assignVars t

expandWord :: Word -> InterpreterIO [T.Text]
expandWord w =
  case w of-- TODO

expandWords :: [Word] -> InterpreterIO [T.Text]
expandWords []      = return []
expandWords (h : t) = (flip (++)) <$> expandWord h <*> expandWords t

interpretCommand :: Command -> InterpreterIO T.Text
interpretCommand (SimpleCommand asgs []) = T.empty <$ (assignVars asgs)

interpretCommand (SimpleCommand asgs words) = do
  Env varsRef <- ask
  vars <- readIORef varsRef
  args <- expandWords words
  writeIORef varsRef vars
  res

interpretCommand (CompoundCommand clause) = undefined
  

interpretCompleteCommand :: CompleteCommand -> InterpreterIO T.Text
interpretCompleteCommand = undefined

interpret :: CompleteCommand -> Env -> InterpreterIO ()
interpret cmd env = undefined
