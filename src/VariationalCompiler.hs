{-# LANGUAGE OverloadedStrings #-}

module VariationalCompiler where

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Data.Scientific as Scientific
import Data.Aeson
import Data.Text(pack)

-- Abstract syntax

data Region = Region [Segment] (Int, Int) (Int, Int)
              deriving(Show)

type Dimension = String
data Segment = Choice Dimension Region Region
             | Text String
               deriving(Show)
type Program = [Segment]


toPoint :: (Int, Int) -> [Scientific]
toPoint t = [toScientific (fst t), toScientific (snd t)]
        where toScientific i = scientific (fromIntegral i) 0

instance ToJSON Segment where
  toJSON (Text s) = object
    [ "type" .= pack "java-segment"
    , "content" .= s
    ]

  toJSON (Choice i l r) = object
    [ "type" .= pack "dimension"
    , "id" .= i
    , "left" .= l
    , "right" .= r
    ]

-- Parser

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar <* sc

dimension :: Parser Segment
dimension =
  do s <- getPosition
     rword "#dimension" <?> "Choice keyword"
     name <- many alphaNumChar
     p1 <- vjProgram
     rword "#else" <?> "Else keyword"
     p2 <- vjProgram
     rword "#end" <?> "End keyword"
     e <- getPosition
     return (Choice name p1 p2 (fromSourcePos s) (fromSourcePos e))
  <?> "Choice node"

javaSegment :: Parser Segment
javaSegment = do
  b <- getPosition -- Get the position for the beginning of the segment
  s <- someTill anyChar
    (lookAhead
      (choice
        [void (string "#dimension" <?> "Choice lookahead")
        , void (string "#else" <?> "Else lookahead")
        , void (string "#end" <?> "End lookahead")
        , eof]))
  a <- getPosition
  return (Text (fromSourcePos b) (fromSourcePos a) s)

fromSourcePos :: SourcePos -> (Int, Int)
fromSourcePos b = (sourceLine b,sourceColumn b)

vjSegment :: Parser Segment
vjSegment = choice [dimension, javaSegment]

vjProgram :: Parser Program
vjProgram = manyTill vjSegment
  (lookAhead
    (choice
      [void (string "#else" <?> "Else lookahead")
      , void (string "#end" <?> "End lookahead")
      , eof]))
