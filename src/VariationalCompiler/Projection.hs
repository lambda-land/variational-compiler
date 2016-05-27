module VariationalCompiler.Projection where

-- | Reduces a VJProgram given a list of choices.
view :: [Selection] -> VJProgram -> VJProgram
view cs = concatMap (viewSegment cs)

-- | Reduces a VJSegment given a list of choices.
viewSegment :: [Selection] -> VJSegment -> VJProgram
viewSegment cs (Choice i l r) = case lookup i cs of
        Nothing          -> [Choice i (view cs l) (view cs r)]
        Just SelectLeft  -> view cs l
        Just SelectRight -> view cs r
-- It's assumed that anything besides a dimension will just be a JavaSegment.
viewSegment _ js = [js]
