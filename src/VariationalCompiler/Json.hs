{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module VariationalCompiler.Json where

import VariationalCompiler.Entities (Segment(..),Program(..),Dimension, Region,Alternative)
import Control.Monad.State.Class
import Control.Monad.State
import Control.Monad
import Data.Functor.Identity
import Text.Megaparsec
import Data.Aeson
import Data.Text(pack)
import GHC.Generics
import Data.Aeson.TH
import Data.ByteString.Lazy (ByteString)

-- | Prepare the list and then call toJSON on the type with
--   begin and end data.
instance ToJSON Program where
    toJSON = toJSON . jsonPrepare

-- | Parse the type with begin and end data and then strip
--   the extra data.
instance FromJSON Program where
    parseJSON o = parseJSON o >>= return . jsonUnprepare

-- | For converting parsec SourcePos to json
instance ToJSON SourcePos where
  toJSON pos = object
    [ "sourceName" .= sourceName pos
    , "sourceLine" .= sourceLine pos
    , "sourceColumn" .= sourceColumn pos
    ]

-- | For converting parsec Message to json
instance ToJSON Message where
  toJSON = toJSON . messageString

-- | For convertng parsec ParseError to json
instance ToJSON ParseError where
  toJSON err = object
      [ "errorPos" .= errorPos err
      , "errorMessages" .= errorMessages err
      ]

-- Slightly Different Definitions that contains a range around the region
-- These are declared using record syntax to take advantage of GHC generics for serialization.
type Loc = [Int] -- [Int, Int]
type Program' = [Segment']
data Segment' = Choice'
                    { dimension :: Dimension
                    , left :: Region'
                    , right :: Region'
                    }
              | Text' { content :: String }
               deriving(Show,Generic)
data Region' = Region'
                   { region :: [Segment']
                   , start :: [Int]
                   , end :: [Int]
                   }
              deriving(Show,Generic)

-- | Options that will be passed into the generated aeson instances
customOptions = defaultOptions
        { sumEncoding = defaultTaggedObject
                            { tagFieldName = "type"
                            , contentsFieldName = "c" -- Shouldn't show up any where
                            }
        , constructorTagModifier = tm
        }
      where tm "Choice'" = "choice"
            tm "Text'" = "text"

-- Automatic generated instances with the custom settings applied
instance ToJSON Segment' where
    toEncoding = genericToEncoding customOptions
instance ToJSON Region' where -- Appears that if this wasn't set it would break all the lower ones
    toEncoding = genericToEncoding customOptions
instance ToJSON Alternative

-- Automatically generated instances that presumably use the ToJSON instances
instance FromJSON Segment'
instance FromJSON Region'
instance FromJSON Alternative

-- | Run the state monad on the program to generate the start and end data
jsonPrepare :: Program -> Program'
jsonPrepare (P p) = case runState (regionPrepare p) [0,0] of
                      (Region' s _ _, _) -> s

-- | Monad that adds start and end information to regions by retreiving the
--   start and end from the state monad before and after preparing the
--   segments.
regionPrepare :: [Segment] -> StateT Loc Identity Region'
regionPrepare s = do start <- get
                     reg <- mapM segmentPrepare s
                     end <- get
                     return (Region' reg start end)

-- | Monad that prepares statements by executing a prepare on the region for
--   choice segments or by adjusting the state for text statements
segmentPrepare :: Segment -> StateT Loc Identity Segment'
segmentPrepare (Choice d p1 p2) = do r1 <- regionPrepare p1
                                     r2 <- regionPrepare p2
                                     return (Choice' d r1 r2)
segmentPrepare (Text s)         = do modify (adjLoc s)
                                     return (Text' s)

-- | Strip all start and end info from the node to create a normal Program
jsonUnprepare :: Program' -> Program
jsonUnprepare = P . fmap segmentUnprepare

-- | Convert to the normal type versions, recursivly calling into regionUnprepare to
--   remove data from decending nodes
segmentUnprepare :: Segment' -> Segment
segmentUnprepare (Choice' d l r) = Choice d (regionUnprepare l) (regionUnprepare r)
segmentUnprepare (Text' s) = Text s

-- | Removes the start and end data from the region
regionUnprepare :: Region' -> Region
regionUnprepare (Region' s _ _) = fmap segmentUnprepare s

-- | Change the location based on the characters in the string. When a new line character is
--   encountered the line count is incremented and the column count is reset.
adjLoc :: String -> Loc -> Loc
adjLoc s i = foldl beanCounter i s
      where beanCounter :: Loc -> Char -> Loc
            beanCounter [pl, _]  '\n' = [pl + 1, 0]
            beanCounter [pl, pc] _    = [pl, pc + 1]
