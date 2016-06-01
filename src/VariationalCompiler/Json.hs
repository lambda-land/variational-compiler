{-# LANGUAGE OverloadedStrings #-}
module VariationalCompiler.Json where

import VariationalCompiler.Entities
import Control.Monad.State.Class
import Control.Monad.State
import Control.Monad
import Data.Functor.Identity
import Text.Megaparsec
import Data.Aeson
import Data.Text(pack)

instance ToJSON Segment' where
  toJSON (Text' s) = object
    [ "type" .= pack "java-segment"
    , "content" .= s
    ]

  toJSON (Choice' i l r) = object
    [ "type" .= pack "dimension"
    , "id" .= i
    , "left" .= l
    , "right" .= r
    ]

instance ToJSON Region' where
--[Segment] (Int, Int) (Int, Int)
   toJSON (Region' s (sl,sc) (el,ec)) = object
    [ "start"  .= [sl,sc]
    , "end"    .= [el,ec]
    , "region" .= s
    ]

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
type Program' = [Segment']
data Segment' = Choice' Dimension Region' Region'
              | Text' String
               deriving(Show)
data Region' = Region' [Segment'] (Int, Int) (Int, Int)
              deriving(Show)

jsonPrepare :: Program -> Program'
jsonPrepare p = case runState (region p) (0,0) of
                      (Region' s _ _, _) -> s

type Loc = (Int, Int)

region :: [Segment] -> StateT Loc Identity Region'
region s = do start <- get
              reg <- mapM segment s
              end <- get
              return (Region' reg start end)

segment :: Segment -> StateT Loc Identity Segment'
segment (Choice d p1 p2) = do r1 <- region p1
                              r2 <- region p2
                              return (Choice' d r1 r2)
segment (Text s)         = do modify (adjLoc s)
                              return (Text' s)

-- | Adjust location based on the string
--
-- >>> adjLoc "    hello world" (0,0)
-- (0,15)
-- >>> adjLoc "public class HelloWorld {\n\n    public static void main(String[] args) {\n        // Prints \"Hello, World\" to the terminal window.\n        " (0,0)
-- (4,8)
adjLoc :: String -> Loc -> Loc
adjLoc s i = foldl beanCounter i s
      where beanCounter :: Loc -> Char -> Loc
            beanCounter (pl, _)  '\n' = (pl + 1, 0)
            beanCounter (pl, pc) _    = (pl, pc + 1)
