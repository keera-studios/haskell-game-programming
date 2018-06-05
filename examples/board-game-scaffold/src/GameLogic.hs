-- | The logic of this board game scaffold contains two parts.
--
-- First, the general game flow with (optioanl) adjustment of the `raw` user
-- input to game logic specific user input and the matching of the game states
-- with the game states specific functions.
--
-- Second, the game state specific functions like the update of a grid, with
-- respect to sensible user input.
module GameLogic where

-- External imports

-- Internal imports
import GameState        (GameState, GameStatus (LevelPlaying), gameGrid,
                         gameInfo, gameStatus)
import InputOutputMatch (clickedFields)
import Objects          (Field, FieldKind (Empty, Tile), Grid, fieldKind,
                         swapFields)
import UserInput        (Controller, controllerDragPoss)

-- * General game flow

-- | Process input from a user and produce new game state.

-- If you wonder where the arguments are, search for `eta conversion`.
gameLogic :: Controller -> GameState -> GameState
gameLogic = updateGameState -- Place for adapting the controller, if needed.

-- | Update a recent game state based on the user input (controller).
updateGameState :: Controller ->  GameState -> GameState
updateGameState ctrl gs = case status of
  LevelPlaying -> playLevel ctrl gs
  _            -> gs
  where
    status = gameStatus (gameInfo gs)

-- * Game state specific updates

-- | Change the game state within playing a level with respect
-- to the controller (user input).

-- If you wonder where the arguments are, search for `eta conversion`.
playLevel :: Controller -> GameState -> GameState
playLevel = updateGrid -- Place to changing the game status (i.e., solved).

-- ** Grid Update

-- | Update the grid if the last two clicks were valid.
-- That is, the first was a click on a tile, the second on an empty field.
updateGrid :: Controller -> GameState -> GameState
updateGrid ctrl gs = gs'
  where
    (fstPos, sndPos)   = controllerDragPoss ctrl
    maybeClickedFields = validClickedFields fstPos sndPos (gameGrid gs)
    gs'                = makeMove gs maybeClickedFields

-- | Return clicked fields, if the first click was on a tile and the second
-- on an empty field.

-- We use pattern guards in this function to avoid the manual handling of 'isJust'.
validClickedFields :: (Int, Int) -> (Int, Int) -> Grid -> Maybe (Field, Field)
validClickedFields click1 click2 grid | Just f1 <- mf1
                                      , Just f2 <- mf2
                                      , Tile == fieldKind f1 , Empty == fieldKind f2
                                        = Just (f1, f2)
                                      | otherwise = Nothing
  where
    (mf1, mf2) = clickedFields click1 click2 grid

-- | Swap two fields in the game grid of a game state.
makeMove :: GameState -> Maybe (Field, Field) -> GameState
makeMove gameState Nothing = gameState
makeMove gameState (Just (f1, f2)) = gameState { gameGrid = grid' }
  where
    grid  = gameGrid gameState
    grid' = swapFields f1 f2 grid
