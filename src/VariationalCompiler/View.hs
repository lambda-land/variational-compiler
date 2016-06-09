module VariationalCompiler.View where
import VariationalCompiler.Entities
import Data.List

-- | Same as 'view' but handles the Program type
getView :: Projection -> Program
getView Projection { program = (P p), selections = cs } = P (view cs p)

-- | Reduces a set of a program given a list of choices.
view :: [Selection] -> [Segment] -> [Segment]
view cs = concatMap (viewSegment cs)


-- | Reduces a segment given a list of choices.
viewSegment :: [Selection] -> Segment -> [Segment]
viewSegment cs (Choice i l r) = case find (\Selection {dimension = d} -> d == i) cs of
        Nothing -> [Choice i (view cs l) (view cs r)]
        Just Selection { alternative = LeftBranch }  -> view cs l
        Just Selection { alternative = RightBranch } -> view cs r
-- It's assumed that anything besides a dimension will just be a text segment.
viewSegment _ js = [js]
