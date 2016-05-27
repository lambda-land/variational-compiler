module VariationalCompiler.Entities where

-- Abstract Syntax Tree

-- | A dimension is an identifier that is assigned to a region 
--   with a choice. A set of dimensions are specified to generate
--   a view.
type Dimension = String 

-- | A program is the root of the abstract syntax tree that represents
--   the variational file.
type Program = [Segment]

-- | A segment represents a portion of a file. Either a literal 
--   portion code or a variational portion of the file are given.
data Segment = Choice Dimension Region Region
             | Text String
               deriving(Show)

-- | A series of segments with information about the starting and 
--   ending location (Line, Column) of the segments in the program.
data Region = Region [Segment] (Int, Int) (Int, Int)
              deriving(Show)


-- Selection

data Alternative = LeftBranch | RightBranch

type Selection = (Dimension, Alternative)


