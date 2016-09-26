{-# LANGUAGE DuplicateRecordFields #-}

module VariationalCompiler.View where
import VariationalCompiler.Entities
import Data.List

-- | Same as 'view' but handles the Program type
viewProjection :: Projection -> [Segment]
viewProjection Projection { program = p, selections = cs } = view cs p

-- | Reduces a set of a program given a list of choices.
view :: [Selection] -> [Segment] -> [Segment]
view cs = concatMap (viewSegment cs)

-- | Reduces a segment given a list of choices.
viewSegment :: [Selection] -> Segment -> [Segment]
viewSegment cs (ChoiceSeg (Choice cdim l r sp)) = case find (\Selection { dimension = sdim} -> sdim == cdim) cs of
        Nothing -> [ChoiceSeg $ Choice cdim (view cs l) (view cs r) sp]
        Just Selection { alternative = LeftBranch }  -> view cs l
        Just Selection { alternative = RightBranch } -> view cs r
-- It's assumed that anything besides a dimension will just be a text segment.
viewSegment _ js = [js]
