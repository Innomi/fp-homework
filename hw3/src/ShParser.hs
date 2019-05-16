module ShParser
  ( completeCommandP
  ) where

import qualified Data.Text as T (concat, pack, singleton, Text)
import Data.Void (Void)
import Prelude hiding (Word)

import Text.Megaparsec (between, empty, eof, many, noneOf, Parsec, some, takeWhileP, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, newline, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme, space, symbol)

import ShGrammar

type Parser = Parsec Void T.Text

-- Lexer

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser T.Text
symbol = (L.symbol spaceConsumer) . T.pack

separatorOpT :: Parser T.Text
separatorOpT = symbol ";"

pipeT :: Parser T.Text
pipeT = symbol "|"

ifT :: Parser T.Text
ifT = symbol "if"

thenT :: Parser T.Text
thenT = symbol "then"

elifT :: Parser T.Text
elifT = symbol "elif"

elseT :: Parser T.Text
elseT = symbol "else"

fiT :: Parser T.Text
fiT = symbol "fi"

whileT :: Parser T.Text
whileT = symbol "while"

doT :: Parser T.Text
doT = symbol "do"

doneT :: Parser T.Text
doneT = symbol "done"

newlineT :: Parser T.Text
newlineT = lexeme (T.singleton <$> newline)

assignT :: Parser Assignment
assignT = lexeme (Assignment <$> nameT <*> (char '=' *> (Word <$> (many wordPartT))))

wordT :: Parser Word
wordT = lexeme (Word <$> wordTImpl)

wordTImpl :: Parser [WordPart]
wordTImpl = some wordPartT

wordPartT :: Parser WordPart
wordPartT = TextPart <$> (T.concat <$> (some wordTextPartT))
        <|> DollarPart <$> dollarT
        <|> char '\"' *> (DoubleQuotedPart <$> doubleQuotedT) <* char '\"'

wordTextPartT :: Parser T.Text
wordTextPartT = (T.singleton) <$> noneOf ['$', '\\', '\'', '\"', '(', ')', ';', ' ', '\n']
            <|> (T.pack "$")  <$ string (T.pack "\\$")
            <|> (T.pack "\\") <$ string (T.pack "\\\\")
            <|> (T.pack "\"") <$ string (T.pack "\\\"")
            <|> (T.pack "\'") <$ string (T.pack "\\\'")
            <|> (T.pack ";")  <$ string (T.pack "\\;")
            <|> between (char '\'') (char '\'') (takeWhileP Nothing (\c -> c /= '\''))

doubleQuotedT :: Parser DoubleQuoted
doubleQuotedT = DoubleQuoted <$> doubleQuotedTImpl

doubleQuotedTImpl :: Parser [DoubleQuotedPartPart]
doubleQuotedTImpl = some doubleQuotedPartT

doubleQuotedPartT :: Parser DoubleQuotedPartPart
doubleQuotedPartT = DollarDoubleQuotedPart <$> dollarT
                <|> TextDoubleQuotedPart   <$> (T.concat <$> (some textDoubleQuotedPartT))

textDoubleQuotedPartT :: Parser T.Text
textDoubleQuotedPartT = (T.pack "\"") <$ string (T.pack "\\\"")
                    <|> (T.pack "$")  <$ string (T.pack "\\$")
                    <|> (T.pack "\\") <$ string (T.pack "\\\\")
                    <|> (T.singleton) <$> noneOf ['$', '\\', '\"']

dollarT :: Parser Dollar
dollarT = char '$' *> (Var    <$> nameT 
                   <|> Inline <$> between (char '(') (char ')') compoundListP)

nameT :: Parser Name
nameT = Name <$> (T.pack <$> (some (alphaNumChar <|> char '_')))

-- Parser

--programP :: Parser ()
--programP = linebreakP *> completeCommandsP <* linebreakP
--       <|> linebreakP

--completeCommandsP :: Parser ()
--completeCommandsP = completeCommandsP *> newlineListP *> completeCommandP
--                <|>                                      completeCommandP

completeCommandP :: Parser CompleteCommand
completeCommandP = CompleteCommand <$> (((:) <$> pipelineP <*> 
                                                 many (try (separatorOpT *> pipelineP)))
                                                 <* (try separatorOpT <|> mempty)
                                                 <* (try newlineListP <|> eof))

pipelineP :: Parser Pipeline
pipelineP = Pipeline <$> ((:) <$> commandP <*> many (try (pipeT *> linebreakP *> commandP)))

commandP :: Parser Command
commandP = simpleCommandP
       <|> compoundCommandP

simpleCommandP :: Parser Command
simpleCommandP = try (SimpleCommand <$> (try cmdPrefixP <|> mempty) <*> cmdSuffixP)
             <|> SimpleCommand <$> cmdPrefixP <*> (try cmdSuffixP <|> mempty)

cmdPrefixP :: Parser [Assignment]
cmdPrefixP = some (try assignT)

cmdSuffixP :: Parser [Word]
cmdSuffixP = some (try wordT)

compoundCommandP :: Parser Command
compoundCommandP = CompoundCommand <$> ifClauseP
               <|> CompoundCommand <$> whileClauseP

ifClauseP :: Parser Clause
ifClauseP = If <$> ifClausePImpl

ifClausePImpl :: Parser [CompleteCommand]
ifClausePImpl = (:)  <$> (ifT *> compoundListP) <*> 
                ((:) <$> (thenT *> compoundListP) <*> 
                ((try elsePartP <|> mempty) <* fiT))

elsePartP :: Parser [CompleteCommand]
elsePartP = try ((:) <$> (elifT *> compoundListP) <*>
                ((:) <$> (thenT *> compoundListP) <*> 
                (try elsePartP <|> mempty)))
        <|> (: []) <$> (elseT *> compoundListP)

whileClauseP :: Parser Clause
whileClauseP = While <$> (whileT *> compoundListP) <*> doGroupP

doGroupP :: Parser CompleteCommand
doGroupP = doT *> compoundListP <* doneT

compoundListP :: Parser CompleteCommand
compoundListP = CompleteCommand <$> (linebreakP *> termP <* (try sequentialSepP <|> mempty))

termP :: Parser [Pipeline]
termP = (:) <$> pipelineP <*> many (try (sequentialSepP *> pipelineP))

sequentialSepP :: Parser ()
sequentialSepP = separatorOpT *> linebreakP
             <|> newlineListP

linebreakP :: Parser ()
linebreakP = newlineListP
         <|> mempty

newlineListP :: Parser ()
newlineListP = () <$ (some newlineT)
