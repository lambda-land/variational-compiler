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

toPoint :: (Int, Int) -> [Scientific]
toPoint t = [toScientific (fst t), toScientific (snd t)]
        where toScientific i = scientific (fromIntegral i) 0

-- Parser

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar <* sc

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

fromSourcePos :: SourcePos -> (Int, Int)
fromSourcePos b = (sourceLine b,sourceColumn b)

segment :: Parser Segment
segment = choice [choiceSegment, textSegment]

region :: Parser [Segment]
region = manyTill segment
  (lookAhead
    (choice
      [void (string "#else" <?> "Else lookahead")
      , void (string "#end" <?> "End lookahead")
      , eof]))

program :: Parser Program
program = region >>= return . P
