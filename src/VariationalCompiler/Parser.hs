module VariationalCompiler.Parser where

import VariationalCompiler.Entities
import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Data.Scientific as Scientific
import Data.Aeson
import Data.Text(pack)
import Data.Functor.Identity

-- Parser

-- | Consume white space.
sc :: Parser ()
sc = L.space (void spaceChar) empty empty

-- | Consume a reserved word w followed by a trailing whitespace character.
rword :: String -> Parser ()
rword w = do
  string w
  void spaceChar

getLineCol :: ParsecT Dec String Identity LineCol
getLineCol = fromSourcePos <$> getPosition

-- | Parse the concrete choice syntax into Segment node in ast
choiceSegment :: Parser Segment
choiceSegment =
  do start <- getLineCol
     rword "#dimension" <?> "Choice keyword"
     name <- many alphaNumChar
     char '\n'
     r1 <- region
     rword "#else" <?> "Else keyword"
     r2 <- region
     rword "#end" <?> "End keyword"
     end <- getLineCol
     return (ChoiceSeg $ Choice name r1 r2 (Span start end))
  <?> "Choice node"

-- | Parse input into string until a choice keyword is encountered
--   then return the string as a text segment.
textSegment :: Parser Segment
textSegment = do
  start <- getLineCol
  s <- someTill anyChar
    (lookAhead
      (choice
        [void (string "#dimension" <?> "Choice lookahead")
        , void (string "#else" <?> "Else lookahead")
        , void (string "#end" <?> "End lookahead")
        , eof]))
  end <- getLineCol
  return (ContentSeg $ Content s (Span start end))

-- | Parse either a choice or a text segment
segment :: Parser Segment
segment = choice [choiceSegment, textSegment]

-- | Parse a series of segments (a region or a program
region :: Parser [Segment]
region = manyTill segment
  (lookAhead
    (choice
      [void (string "#else" <?> "Else lookahead")
      , void (string "#end" <?> "End lookahead")
      , eof]))

-- | Main parser for variational programs.
program :: Parser [Segment]
program = region
