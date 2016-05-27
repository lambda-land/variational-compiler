{-# LANGUAGE OverloadedStrings #-}
module VariationalCompiler.Json where

import VariationalCompiler.Entities
import Text.Megaparsec
import Data.Aeson
import Data.Text(pack)

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

instance ToJSON Region where
--[Segment] (Int, Int) (Int, Int)
   toJSON (Region s (sl,sc) (el,ec)) = object
    [ "start"  .= [sl,sc]
    , "end"    .= [el,ec]
    , "region" .= s
    ]

instance ToJSON SourcePos where
  toJSON pos = object [
      "sourceName" .= sourceName pos
    , "sourceLine" .= sourceLine pos
    , "sourceColumn" .= sourceColumn pos
    ]

instance ToJSON Message where
  toJSON = toJSON . messageString

instance ToJSON ParseError where
  toJSON err = object [
      "errorPos" .= errorPos err,
      "errorMessages" .= errorMessages err
    ]

