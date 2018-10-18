-- | The state of the game during execution. It has two parts:
--
--   - current game grid
--   - general game info (status, level, lives, remaining moves, etc.).
module GameState where

-- External imports

-- Internal imports
import Constants (initialLevel)
import Levels    (initialGrid, levels)
import Objects   (Grid)

-- * Game states and game statuses

-- | The current state is given by the current grid and the current general
-- 'GameInfo'. The latter contains information regarding the current level.
data GameState = GameState
  { gameGrid :: Grid     -- ^ Current grid.
  , gameInfo :: GameInfo -- ^ Game state, current level, game and level moves.
  } deriving Show

-- | The game information contains the current game state (loading, playing,
-- etc.) and general information (e.g., the current level, lives, number of
-- made or remaining moves).
data GameInfo = GameInfo
  { gameStatus :: GameStatus
  , gameLevel  :: Int
  } deriving Show

-- | Possible game statuses. The game is always in one of these.
-- Interaction and presentation depend on this. The display module changes
-- presentation depending on the status.
data GameStatus = GameStarted      -- ^ Game started, but no level was played yet.
                | GameFinished     -- ^ Game is finished.
                | LevelLoading Int -- ^ Loading a level.
                | LevelPlaying     -- ^ Playing a level.
                | LevelSolved      -- ^ Level is solved.
 deriving (Show, Eq)

-- * Default state

-- | Default game state.
--
-- This game scaffold only uses the level playing status.
-- Therefore the default state is a `neutral` level playing status.
defaultState :: GameState
defaultState = GameState
  { gameGrid = initialGrid $ levels !! initialLevel
  , gameInfo = defaultInfo
  }

-- | Default game info.
--
-- This game scaffold only uses the level playing status.
-- Therefore the default info is a `neutral` level playing info.
defaultInfo :: GameInfo
defaultInfo = GameInfo
  { gameStatus = LevelPlaying
  , gameLevel  = initialLevel
  }
