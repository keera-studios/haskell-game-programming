-- | The state of the game during execution. It has two
-- parts: general info (level, points, etc.) and
-- the actual gameplay info (objects).
--
-- Because the game is always in some running state
-- (there are no menus, etc.) we assume that there's
-- always some gameplay info, even though it can be
-- empty.
module GameState where

-- import FRP.Yampa as Yampa

import Debug.Trace
import Objects
import FRP.Yampa (Time)

-- | The running state is given by a bunch of 'Objects' and the current general
-- 'GameInfo'. The latter contains info regarding the current level, the number
-- of points, etc.
--
-- Different parts of the game deal with these data structures.  It is
-- therefore convenient to group them in subtrees, even if there's no
-- substantial difference betweem them.
data GameState = GameState
  { gameObjects :: !Objects
  , gameInfo    :: !GameInfo
  }

-- | Initial (default) game state.
neutralGameState :: GameState
neutralGameState = GameState
  { gameObjects = []
  , gameInfo    = GameInfo 0
  }

data GameInfo = GameInfo { gameTime :: Time }
