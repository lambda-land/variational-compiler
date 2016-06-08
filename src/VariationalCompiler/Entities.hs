{-# LANGUAGE DeriveGeneric #-}
module VariationalCompiler.Entities where
import GHC.Generics

-- Abstract Syntax Tree

-- | A dimension is an identifier that is assigned to a region 
--   with a choice. A set of dimensions are specified to generate
--   a view.
type Dimension = String 

-- | A program is the root of the abstract syntax tree that represents
--   the variational file.
newtype Program = P [Segment] -- NOTE: This newtype allows us to create an
                              --       instance of the ToJSON and FromJSON
                              --       to add and remove start and end info 
                              --       while serializing and deserializing.
                              --       Otherwise Region and Program would be 
                              --       the same.
               deriving(Show)

-- | A segment represents a portion of a file. Either a literal 
--   portion code or a variational portion of the file are given.
data Segment = Choice Dimension Region Region
             | Text String
               deriving(Show)

-- | Same as a program only logically means the region inside of 
--   a variational statement
type Region = [Segment]


-- Selection

data Alternative = LeftBranch | RightBranch
               deriving(Show, Generic)

type Selection = (Dimension, Alternative)

-- Projection (used when generating a view)
type Projection = (Program, [Selection])
