module VariationalCompilerProjection where
import VariationalCompiler

-- | This can't be named just left and right because those names are taken in the Prelude.
data Alternative = SelectLeft | SelectRight
type Selection = (Dimension, Alternative)

-- | Reduces a VJProgram given a list of choices.
-- >>> view [("x",ALeft)] [Dimension "x" [JavaSegment "hello"] [JavaSegment "world"]]
-- [JavaSegment "hello"]
--
-- >>> view [("DEBUG",ARight)] [Dimension "VERBOSE" [JavaSegment "print or something"] [JavaSegment "silence is golden"]]
-- [Dimension "VERBOSE" [JavaSegment "print or something"] [JavaSegment "silence is golden"]]
--
-- >>> view [("b",ALeft)] p1
-- [JavaSegment "hello",Dimension "a" [JavaSegment "something here"] [JavaSegment "there's this too"],JavaSegment "goodbye"]
view :: [Selection] -> VJProgram -> VJProgram
view cs = concatMap (viewSegment cs)

-- TODO: the Dimension data constructor should be called Choice
-- the Id in Dimension right now is actually the Dimension
-- what's currently called Choice is...whatever I want that's not taken already
-- Maybe a selector.

-- | Reduces a VJSegment given a list of choices.
viewSegment :: [Selection] -> VJSegment -> VJProgram
viewSegment cs (Choice i l r) = case lookup i cs of
  Nothing -> [Choice i (view cs l) (view cs r)]
  Just SelectLeft -> view cs l
  Just SelectRight -> view cs r
-- It's assumed that anything besides a dimension will just be a JavaSegment.
viewSegment _ js = [js]


-- Test values

p1 :: VJProgram
p1 = [Text "hello"
     , Choice "a"
                 [Choice "b"
                            [Text "something here"]
                            []
                 ]
                 [Text "there's this too"]
                 , Text "goodbye"]
