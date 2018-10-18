-- | A scaffold for a board game using SDL2 as a starting point and teaching
-- source for a wide range of board games written in Haskell.
--
-- Author     : Christina Zeller
--
-- Maintainer : chriz@keera.co.uk
--
-- Copyright  : (c) Keera Studios Ltd, 2018
--
-- License    : GPL-3
--
-- Initializes input and output device. Then the game loop is started with a
-- default game state and loops forever with: sensing user input, adapting the
-- game state with respect to the user input, and rendering the current game
-- state.
--
module Main where

-- External imports

-- Internal imports
import DeviceOutput (RenderingCtx, initializeGUI, render)
import GameLogic    (gameLogic)
import GameState    (GameState, defaultState)
import UserInput    (Controller, defaultController, updateController)

-- | Starts the game.
main :: IO ()
main = do
  -- Initialize input device
  let ctrl = defaultController -- In this case it is `only` a default controller.
                               -- The initializing of other input devices (e.g.,
                               -- wiimote)might needs more consideration. Here is
                               -- the place for initializing input devices.

  -- Initialize output device
  ctx <- initializeGUI

  -- Initialize the game with neutral starting states
  gameLoop ctrl ctx defaultState

-- | A standard game loop with sensing user input, adapting the game state and
-- rendering the new game state.
gameLoop :: Controller -> RenderingCtx -> GameState -> IO ()
gameLoop ctrl ctx gs = do
  -- Sense user input
  ctrl' <- updateController ctrl

  -- Adapt the game state with respect to the user input
  let gs' = gameLogic ctrl' gs

  -- Render current game state
  render gs' ctx

  -- Loop
  gameLoop ctrl' ctx gs'
