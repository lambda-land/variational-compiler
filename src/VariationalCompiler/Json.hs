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

instance ToJSON Program where
    toJSON = toJSON . jsonPrepare

instance FromJSON Program where
    parseJSON o = parseJSON o >>= return . jsonUnprepare

instance ToJSON SourcePos where
  toJSON pos = object 
    [ "sourceName" .= sourceName pos
    , "sourceLine" .= sourceLine pos
    , "sourceColumn" .= sourceColumn pos
    ]

instance ToJSON Message where
  toJSON = toJSON . messageString

instance ToJSON ParseError where
  toJSON err = object 
      [ "errorPos" .= errorPos err
      , "errorMessages" .= errorMessages err
      ]

--Slightly Different Definition that contains a range around the region
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

customOptions = defaultOptions 
        { sumEncoding = defaultTaggedObject 
                            { tagFieldName = "type"
                            , contentsFieldName = "c" -- Shouldn't show up any where 
                            }
        , constructorTagModifier = tm
        }
      where tm "Choice'" = "choice"
            tm "Text'" = "text"


instance ToJSON Segment' where
    toEncoding = genericToEncoding customOptions
instance ToJSON Region' where -- Appears that if this wasn't set it would break all the lower ones
    toEncoding = genericToEncoding customOptions
instance ToJSON Alternative

instance FromJSON Segment' 
instance FromJSON Region' 
instance FromJSON Alternative

jsonPrepare :: Program -> Program'
jsonPrepare (P p) = case runState (regionPrepare p) [0,0] of
                      (Region' s _ _, _) -> s


regionPrepare :: [Segment] -> StateT Loc Identity Region'
regionPrepare s = do start <- get
                     reg <- mapM segmentPrepare s
                     end <- get
                     return (Region' reg start end)

segmentPrepare :: Segment -> StateT Loc Identity Segment'
segmentPrepare (Choice d p1 p2) = do r1 <- regionPrepare p1
                                     r2 <- regionPrepare p2
                                     return (Choice' d r1 r2)
segmentPrepare (Text s)         = do modify (adjLoc s)
                                     return (Text' s)

jsonUnprepare :: Program' -> Program
jsonUnprepare = P . fmap segmentUnprepare

segmentUnprepare :: Segment' -> Segment
segmentUnprepare (Choice' d l r) = (Choice d (regionUnprepare l) (regionUnprepare r))
segmentUnprepare (Text' s) = (Text s)

regionUnprepare :: Region' -> Region
regionUnprepare (Region' s _ _) = fmap segmentUnprepare s

adjLoc :: String -> Loc -> Loc
adjLoc s i = foldl beanCounter i s
      where beanCounter :: Loc -> Char -> Loc
            beanCounter [pl, _]  '\n' = [pl + 1, 0]
            beanCounter [pl, pc] _    = [pl, pc + 1]
