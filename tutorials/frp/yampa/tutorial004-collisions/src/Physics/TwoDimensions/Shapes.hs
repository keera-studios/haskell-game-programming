-- | A very simple physics subsytem. It currently detects shape
-- overlaps only, the actual physics movement is carried out
-- in Yampa itself, as it is very simple using integrals and
-- derivatives.
module Physics.TwoDimensions.Shapes where

import FRP.Yampa.VectorSpace
import Physics.TwoDimensions.Dimensions

-- | Side of a rectangle
data Side = TopSide | BottomSide | LeftSide | RightSide
  deriving (Eq,Show)

-- | Opposite side
--
-- If A collides with B, the collision sides on
-- A and B are always opposite.
oppositeSide :: Side -> Side
oppositeSide TopSide    = BottomSide
oppositeSide BottomSide = TopSide
oppositeSide LeftSide   = RightSide
oppositeSide RightSide  = LeftSide

data Shape = -- Rectangle Pos2D Size2D -- A corner and the whole size
             Circle    Pos2D Double -- Position and radius
           | SemiPlane Pos2D Side   -- 

-- | Detects if two shapes overlap.
--
-- Rectangles: overlap if projections on both axis overlap,
-- which happens if x distance between centers is less than the sum
-- of half the widths, and the analogous for y and the heights.

overlapShape :: Shape -> Shape -> Bool
overlapShape (Circle p1 s1) (Circle p2 s2) = (dist - (s1 + s2)) < sigma
  where (dx, dy) = p2 ^-^ p1
        dist     = sqrt (dx**2 + dy**2)
        sigma    = 1
overlapShape (Circle (p1x,p1y) s1) (SemiPlane (px,py) side) = case side of
  LeftSide   -> p1x - s1 <= px
  RightSide  -> p1x + s1 >= px
  TopSide    -> p1y - s1 <= py
  BottomSide -> p1y + s1 >= py
overlapShape s@(SemiPlane _ _) c@(Circle _ _) = overlapShape c s
overlapShape _                 _              = False -- Not really, it's just that we don't care
