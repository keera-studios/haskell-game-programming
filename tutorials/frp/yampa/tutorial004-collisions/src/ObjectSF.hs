-- | Objects as signal functions.
--
-- Live objects in the game take user input and the game universe
-- and define their state in terms of that. They can remember what
-- happened (see Yampa's Arrow combinators, which hide continuations),
-- change their behaviour (see switches in Yampa).
module ObjectSF where

import FRP.Yampa

import Objects
import Input
import Data.IdentityList

-- | Objects are defined as transformations that take 'ObjectInput' signals and
-- return 'ObjectOutput' signals.
type ObjectSF = SF ObjectInput Object

-- | In order to determine its new output, an object needs to know the user's
-- desires ('userInput'), whether there have been any collisions
-- ('collisions').
--
-- The reason for depending on 'Collisions' is that objects may ``change''
-- when hit (start moving in a different direction).
data ObjectInput = ObjectInput
  { userInput    :: Controller
  , collisions   :: Collisions
  }

-- -- | What we can see about each live object at each time. It's a
-- -- snapshot of the object.
-- data ObjectOutput = ObjectOutput
--   { outputObject :: Object   -- ^ The object's state (position, shape, etc.).
--   } 

-- | List of identifiable objects. Used to work with dynamic object
-- collections.
type ObjectSFs = IL ObjectSF

-- extractObjects :: Functor f => SF (f ObjectOutput) (f Object)
-- extractObjects = arr (fmap outputObject)
-- 
-- -- | A list of object outputs
-- type ObjectOutputs = [ObjectOutput]
-- 
