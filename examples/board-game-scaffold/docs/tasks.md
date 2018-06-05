# ZuriHac 2018 --- Game Programming --- Some Tasks
by Christina Zeller

- This document gives an unsorted overview of suggested tasks in Haskell game programming.
- The unsorted tasks vary in complexity. Don't get (too) frustrated but ask me for hints.

Table of Contents:
  * [Hands on tasks (playing with the code)](#hands-on-tasks-playing-with-the-code)
  * [Hands on tasks (createing a board game)](#hands-on-tasks-creating-a-board-game)
  * [Exploring the game structure](#exploring-the-game-structure)
  * [Haskanoid: SDL FRP (Yampa) Wiimote](#haskanoid-sdl-frp-yampa-wiimote)
  * [Confused?](#confused)

## Hands on tasks (playing with the code)
### Constants
- Change the title of the window.
- Change the default screen size.
- Change the screen margin.
### Levels
- Change the background colour.
- Change the fields colour.
- Change the number of fields of a grid.
- Change the shape of the grid.
- Add a new level.
- Add a level solved definition for each level.
- Add a maximal number of moves in a level.
### GameState
- Add a field to count moves.
- Add a pause game state.
### DeviceOutput
- Do not render/display empty fields.
- Do render/display empty fields as small filled pies or ellipses.
- Draw fields as triangles.
- Add rendering/displaying of a level solved status.
- Add rendering/displaying of a paused game status.
- Add a button (rectangle, circle, ...) next to the game board.
- Add the level number next to the game board.
- Render images/assets instead of circles/triangles/rectangles ([one source of free assets](http://www.kenney.nl/)).
- Add audio.
### UserInput
- Close the window if the close button of a window was clicked.
- Handle/Store a keyboard pressed key 'P' event.
### InputOutputMatch
- Add a function to check whether the background was clicked.
- Add a function to check whether a specific area was clicked (e.g., a restart button).
### Objects
- Add a new field kind.
### GameLogic
- Change the allowed moves.
- Change the result of a move.
- Count the number of moves.
- Restart the level if the background or a button on the screen was clicked.
- Add a level solved behaviour.
- Add a pause game behaviour.
- React on a keyboard pressed key 'P' event with changing to the paused game status.

## Hands on tasks (creating a board game)
- Reimplement an existing board game (4 in a line, solitaire, find the pairs, Mensch \"argere dich nicht, ...).
- Create your own board game (using the board game scaffold) with your own logic and rules.

## Exploring the game structure
- Create the haddock documentation and get an overview of the game.
- Where do you expect:
  - The handling of mouse and key board events?
  - The definition of the game board and its elements in general?
  - The definition of a solved game board?
  - The handling of the game flow?
  - The rendering of a game board?
  - The background colour?
  - The colour of fields of the board?
  - The logical board size?
  - The board size used for rendering?
  - The definition of a valid move?
  - The result of a move?
  - The detection of clicked fields?
- Take a look at the following import examples. Can you associate them with the specific modules of the game?
  - Example 1:

```haskell
-- External imports
import           Control.Monad   (void)
import           Data.Int        (Int16, Int32)
import           Data.List       (transpose)
import qualified Data.Text       as T (pack)
import           Foreign.C.Types (CInt (CInt))
import           SDL             (InitFlag (InitVideo), Renderer, V2 (V2),
                                  V4 (V4), Window, createSoftwareRenderer,
                                  createWindow, defaultWindow, getWindowSurface,
                                  getWindowSurface, initialize, showWindow,
                                  surfaceFillRect, updateWindowSurface,
                                  windowInitialSize)
import           SDL.Primitive   (fillCircle)

-- Internal imports
import Constants (Color, caption, defaultBG, screenMarginWH, screenWH)
import GameState (GameState, GameStatus (LevelPlaying), gameGrid, gameInfo,
                  gameLevel, gameStatus)
import Levels    (LevelSpec, background, fieldColor, levels)
import Objects   (Field, Grid, fieldId, findPosition)
```

  - Example 2:

```haskell
-- External imports

-- Internal imports
import GameState        (GameState, GameStatus (LevelPlaying), gameGrid,
                         gameInfo, gameStatus)
import InputOutputMatch (clickedFields)
import Objects          (Field, FieldKind (Empty, Tile), Grid, fieldKind,
                         swapFields)
import UserInput        (Controller, controllerDragPoss)
```

  - Example 3:

```haskell
-- External imports
import Data.Ix (inRange)

-- Internal imports
import DeviceOutput (absFieldsPositions, calcFieldSize)
import Objects      (Field, Grid)
```

  - Example 4:

```haskell
-- External imports
import Data.Ix   (inRange)
import Data.List (find)

-- Internal imports
import DeviceOutput (ScreenPosition, gridFieldSize, gridFieldsAbsPositions)
import Objects      (Field, Grid)
```

  - Example 5:

```haskell
-- External imports

-- Internal imports
import DeviceOutput (RenderingCtx, initializeGUI, render)
import GameLogic    (gameLogic)
import GameState    (GameState, defaultState)
import UserInput    (Controller, defaultController, updateController)
```

  - Example 6:

```haskell
-- External imports
import Data.Int (Int32)
import SDL      (EventPayload (MouseButtonEvent, MouseMotionEvent),
                 InputMotion (Pressed, Released), Point (P), V2 (V2),
                 eventPayload, mouseButtonEventMotion, mouseMotionEventPos,
                 pollEvent)

-- Internal imports
```

- Create a list for commonly used variables and abbreviations in the code. If you see irregularities you are very wellcome to fix them.
- What makes it easy or hard to understand the code? If you want try to fix it and/or give us feedback regarding the difficulty of understanding our documents and our code.
- Compare the board game scaffold with the game structure of [haskanoid](https://github.com/ivanperez-keera/haskanoid) and/or [Pang a Lambda](https://github.com/keera-studios/games-pang-a-lambda).
- Structure [raindrops](https://github.com/keera-studios/haskell-game-programming/tree/master/examples/raindrops) using our basic game structure.

## Haskanoid: SDL FRP (Yampa) Wiimote
- Take a look at the [issues](https://github.com/ivanperez-keera/haskanoid/issues) that we added and tagged with ZuriHac2018 of [haskanoid](https://github.com/ivanperez-keera/haskanoid) our breakout game in Haskell using SDL, functional reactive programming (Yampa) and wiimote support.

## Confused?
Don't panic! Let's talk and find a solution. ;-)
