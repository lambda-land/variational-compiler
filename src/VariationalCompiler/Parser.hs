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

-- Parser

-- | Consume white space.
sc :: Parser ()
sc = L.space (void spaceChar) empty empty

-- | Consume a reserved word w.
rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar <* sc

-- | Parse the concrete choice syntax into Segment node in ast
choiceSegment :: Parser Segment
choiceSegment =
  do rword "#dimension" <?> "Choice keyword"
     name <- many alphaNumChar
     p1 <- region
     rword "#else" <?> "Else keyword"
     p2 <- region
     rword "#end" <?> "End keyword"
     e <- getPosition
     return (Choice name p1 p2)
  <?> "Choice node"

-- | Parse input into string until a choice keyword is encountered
--   then return the string as a text segment.
textSegment :: Parser Segment
textSegment = do
  s <- someTill anyChar
    (lookAhead
      (choice
        [void (string "#dimension" <?> "Choice lookahead")
        , void (string "#else" <?> "Else lookahead")
        , void (string "#end" <?> "End lookahead")
        , eof]))
  return (Text s)

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
program :: Parser Program
program = region >>= return . P
