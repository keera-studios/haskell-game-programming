{-# LANGUAGE FlexibleContexts #-}
-- | A very rudimentary collision system.
--
-- It compares every pair of objects, trying to determine if there is a
-- collision between the two of them.
--
-- NOTE: In order to minimize the number of comparisons, only moving objects
-- are tested (against every game object). That's only 2 objects right now
-- (making it almost linear in complexity), but it could easily grow and become
-- too slow.
--
module Physics.TwoDimensions.GameCollisions where

import           Data.List
import           Data.Maybe
import           Data.IdentityList
import           Physics.TwoDimensions.Collisions
import qualified Physics.TwoDimensions.Collisions      as C
import           Physics.TwoDimensions.PhysicalObjects
import           Physics.TwoDimensions.Shapes

-- | Given a list of objects, it detects all the collisions between them.
--
-- Note: this is a simple n*m-complex algorithm, with n the
-- number of objects and m the number of moving objects (right now,
-- only 2).
--
detectCollisions :: (Eq n , PhysicalObject o n Shape) => IL o -> Collisions n
detectCollisions = detectCollisionsH
 where detectCollisionsH objsT = flattened
         where -- Eliminate empty collision sets
               -- TODO: why is this really necessary?
               flattened = filter (\(C.Collision n) -> not (null n)) collisions

               -- Detect collisions between moving objects and any other objects
               collisions = detectCollisions' objsT moving

               -- Partition the object space between moving and static objects
               (moving, _static) = partition (physObjectCollides.snd) $ assocsIL objsT

-- | Detect collisions between each moving object and
-- every other object.
detectCollisions' :: (Eq n, PhysicalObject o n Shape) => IL o -> [(ILKey, o)] -> [Collision n]
detectCollisions' objsT ms = concatMap (detectCollisions'' objsT) ms

-- | Detect collisions between one specific moving object and every existing
-- object. Each collision is idependent of the rest (which is not necessarily
-- what should happen, but since the transformed velocities are eventually
-- added, there isn't much difference in the end).
detectCollisions'' :: (Eq n, PhysicalObject o n Shape) => IL o -> (ILKey, o ) -> [Collision n]
detectCollisions'' objsT m = concatMap (detectCollisions''' m) (assocsIL objsT)

-- | Detect a possible collision between two objects. Uses the object's keys to
-- distinguish them. Uses the basic 'Object'-based 'detectCollision' to
-- determine whether the two objects do collide.
detectCollisions''' :: (Eq n, PhysicalObject o n Shape) => (ILKey, o) -> (ILKey, o) -> [Collision n]
detectCollisions''' m o
 | fst m == fst o = []    -- Same object -> no collision
 | otherwise      = maybeToList (detectCollision (snd m) (snd o))
