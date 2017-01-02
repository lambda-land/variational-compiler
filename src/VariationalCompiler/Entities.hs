{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module VariationalCompiler.Entities where
import GHC.Generics
import Text.Megaparsec


-------- Abstract Syntax --------

type LineCol = (Int, Int) -- row, column

fromSourcePos :: SourcePos -> LineCol
fromSourcePos (SourcePos _ line column) =
  (fromIntegral $ unPos line - 1, fromIntegral $ unPos column - 1)

-- | Represents the position of a span of text in a source document.
-- Indicates the part of a document a syntax node was parsed from.
data Span = Span
  { start :: LineCol,
    end :: LineCol
  } deriving(Show, Generic)

-- | A part of a document. Contains either simple text content
-- or a choice between two alternatives.
data Segment = ChoiceSeg Choice
             | ContentSeg Content
               deriving(Show)

data Choice = Choice
  { dimension :: String
  , left :: Region
  , right :: Region
  , span :: Span
  } deriving(Show, Generic)

data Content = Content
  { content :: String
  , span :: Span
  } deriving(Show, Generic)

data Region = Region
  { segments :: [Segment]
  , span :: Span
  } deriving(Show, Generic)

-------- View --------

-- | Represents one of the branches of a choice.
data Alternative = LeftBranch | RightBranch
  deriving(Show)

-- | Represents a user's selection of a branch for all choices with a particular dimension.
data Selection = Selection
  { dimension :: String
  , alternative :: Alternative
  } deriving(Show, Generic)

-- Projection (used when generating a view)
--
data Projection = Projection
  { program :: [Segment]
  , selections :: [Selection]
  } deriving(Show, Generic)
