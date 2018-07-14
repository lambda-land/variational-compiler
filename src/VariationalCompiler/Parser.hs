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

getLineCol :: ParsecT Dec String Identity LineCol
getLineCol = fromSourcePos <$> getPosition

-- | Parse the concrete choice syntax into Segment node in ast
choiceSegment :: Parser Segment
choiceSegment =
  do start <- getLineCol
     ck <- choiceKind
     spaceChar
     name <- many alphaNumChar
     r1 <- region
     r2 <- elseBranch
     string "#endif" <?> "string \"#endif\" in choiceSegment"
     end <- getLineCol
     return (ChoiceSeg $ Choice name ck r1 r2 (Span start end))
  <?> "Choice node"

choiceKind :: Parser ChoiceKind
choiceKind =
  try (
    do string "#ifdef" <?> "string \"#ifdef\" in choiceKind"
       return Positive
  ) <|> (
    do string "#ifndef" <?> "string \"#ifndef\" in choiceKind"
       return Contrapositive
  )

elseBranch :: Parser Region
elseBranch =
  try (
    do string "#else"
       region
  ) <|> (
    do start <- getLineCol
       return (Region [] (Span start start)))

-- | Parse input into string until a choice keyword is encountered
--   then return the string as a text segment.
textSegment :: Parser Segment
textSegment = do
  start <- getLineCol
  s <- someTill anyChar
    (lookAhead
      (choice
        [void (string "#ifdef" <?> "string \"#ifdef\" in textSegment")
        , void (string "#ifndef" <?> "string \"#ifndef\" in textSegment")
        , void (string "#else" <?> "string \"#else\" in textSegment")
        , void (string "#endif" <?> "string \"#endif\" in textSegment")
        , eof]))
  end <- getLineCol
  return (ContentSeg $ Content s (Span start end))

-- | Parse either a choice or a text segment
segment :: Parser Segment
segment = choice [choiceSegment, textSegment]

-- | Parse a series of segments (a region or a program
region :: Parser Region
region = do
  start <- getLineCol
  segs <- manyTill segment
    (lookAhead
      (choice
        [void (string "#else" <?> "string \"#else\" in region")
        , void (string "#endif" <?> "string \"#endif\" in region")
        , eof]))
  end <- getLineCol
  return (Region segs (Span start end))

fullSpan :: [Region] -> Span
fullSpan [] = Span (0,0) (0,0)
fullSpan rs =
  let Region _ hsp = head rs
      Region _ lsp = last rs
  in Span (start hsp) (end lsp)

-- | Main parser for variational programs.
program :: Parser Region
program = region

-- TODO: enable consuming extraneous #else and #endif
-- so that more real-world sources can be parsed
