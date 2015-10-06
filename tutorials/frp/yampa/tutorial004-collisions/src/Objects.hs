{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Game objects and collisions.
module Objects where

import Control.Arrow ((***))
import FRP.Yampa.VectorSpace

import qualified Physics.TwoDimensions.Collisions      as C
import           Physics.TwoDimensions.Dimensions
import           Physics.TwoDimensions.PhysicalObjects
import qualified Physics.TwoDimensions.PhysicalObjects as PO
import           Physics.TwoDimensions.Shapes

type Collision  = C.Collision ObjectName
type Collisions = C.Collisions ObjectName

-- * Objects

type Objects = [Object]
type ObjectName = String

-- | Objects have logical properties (ID, kind, dead, hit), shape properties
-- (kind), physical properties (kind, pos, vel, acc) and collision properties
-- (hit, 'canCauseCollisions', energy, displaced).
data Object = Object { objectName           :: !ObjectName
                     , objectKind           :: !ObjectKind
                     , objectPos            :: !Pos2D
                     , objectVel            :: !Vel2D
                     , canCauseCollisions   :: !Bool
                     , collisionEnergy      :: !Double
                     }
 deriving (Show)

-- | The kind of object and any size properties.
data ObjectKind = Ball Double -- radius
                | Side Side
  deriving (Show,Eq)

-- Partial function. Object has size.
objectTopLevelCorner :: Object -> Pos2D
objectTopLevelCorner object = objectPos object ^-^ (half (objectSize object))
  where half = let h = (/2) in (h *** h)

-- Partial function!
objectSize :: Object -> Size2D
objectSize object = case objectKind object of
  (Ball r) -> let w = 2*r in (w, w)

instance PhysicalObject Object String Shape where
  physObjectPos       = objectPos
  physObjectVel       = objectVel
  physObjectElas      = collisionEnergy
  physObjectShape     = objShape
  physObjectCollides  = canCauseCollisions
  physObjectId        = objectName
  physObjectUpdatePos = \o p -> o { objectPos = p }
  physObjectUpdateVel = \o v -> o { objectVel = v }

objShape :: Object -> Shape
objShape obj = case objectKind obj of
  Ball r -> Circle p r
  Side s -> SemiPlane p s
 where p = objectPos obj
