-- | A graphical user interface for the game.
--
-- It consists of three parts.
--
--  - Rendering context
--  - Initialization
--  - Rendering of game states
--
-- The rendering context is used for showing game states on the screen.
-- In SDL2 (which is used in this game) this is a renderer and a window.
--
-- The initialization creates a window that can be seen on the screen and
-- returns the associated rendering context.
--
-- Rendering includes the visualization and sound/music.
--
-- The game state is turned into a background color and visual elements
-- ('visualRepresentation'). The visual representation is then displayed
-- ('display'').
--
-- For the level playing status, the visual elements are the fields of the
-- grid.
--
-- Recently, there is only one type of visual element ('VElement') that is used
-- to define fields with their color, size and position and is displayed as
-- circles ('displayVElement').
--
-- This game does not have sound/music (yet).
module DeviceOutput where

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

-- * Rendering context

-- | The renderer and the window as rendering context.
type RenderingCtx = (Renderer, Window)

-- | Return the renderer of a rendering context.
ctxRenderer :: RenderingCtx -> Renderer
ctxRenderer (r, _w) = r

-- | Return the window of a rendering context.
ctxWindow :: RenderingCtx -> Window
ctxWindow (_r, w) = w

-- * Initialization

-- | Initializing the graphical user interface.
initializeGUI :: IO RenderingCtx
initializeGUI = do
  SDL.initialize [InitVideo]
  window <- SDL.createWindow (T.pack caption) SDL.defaultWindow {SDL.windowInitialSize = V2 screenWH screenWH }
  screenSurface <- SDL.getWindowSurface window
  renderer <- SDL.createSoftwareRenderer screenSurface
  return (renderer, window)

-- * Rendering

-- | Render the game state.

-- If you wonder where the arguments are, search for `eta conversion`.
render :: GameState -> RenderingCtx -> IO ()
render = display -- Audio can be added here.

-- | Display the game state.
display :: GameState -> RenderingCtx -> IO ()
display gs ctx = do
  let vR = visualRepresentation gs
  display' vR ctx

-- | Display a background color and a set of elements.
display':: (Color, [VElement])
       -> RenderingCtx
       -> IO ()
display' (bgColor, vElements) ctx = do

  let window = ctxWindow ctx
  screenSurface <- SDL.getWindowSurface window
  SDL.showWindow window

  let (bgR, bgB, bgG, bgA) = bgColor
  SDL.surfaceFillRect screenSurface Nothing (V4 bgR bgB bgG bgA)

  displayVElements vElements ctx

  SDL.updateWindowSurface window

-- | Display elements as circles.
displayVElements :: [VElement] -> RenderingCtx -> IO ()
displayVElements ves ctx = mapM_ (`displayVElement` ctx) ves

-- | Display one visual element as a circle.
displayVElement :: VElement -> RenderingCtx -> IO ()
displayVElement vElement ctx = do
  let renderer     = ctxRenderer ctx

      (r, g, b, a) = vElementColor vElement
      pos          = vElementPosition vElement
      (w, _h)      = vElementSize vElement
      (x', y', r') = centerCor pos w

  void $ fillCircle renderer (V2 (toCInt x') (toCInt y')) (toCInt r') (V4 r g b a)
  where
    -- | Center coordinates in a squared field.
    -- Given the x and y coordinate of the left upper corner of a field
    -- and its size, return the coordinates of the center and the halved
    -- field size.
    centerCor :: ScreenPosition -> Int -> (Int, Int, Int)
    centerCor (x, y) fsize = (x + fsize', y + fsize', fsize')
      where
        fsize' = fsize `div` 2

-- ** Visual representation of game states

-- | A visual representation of the game state as tuple of a background
-- color and multiple visual elements.
visualRepresentation :: GameState -> (Color, [VElement])
visualRepresentation gs =
  case gameStatus $ gameInfo gs of
    LevelPlaying -> visualRepresentationLevelPlaying gs
    _            -> (defaultBG,[]) -- Add more game states.

-- | A visual representation of the game state, when the game is in the
-- 'LevelPlaying' state, as tuple of a background color and multiple visual
-- elements.
visualRepresentationLevelPlaying :: GameState -> (Color, [VElement])
visualRepresentationLevelPlaying gs = (bgColor, vElements)
  where
    lvlSpec   = levels !! gameLevel (gameInfo gs)
    bgColor   = background lvlSpec
    grid      = gameGrid gs
    fldSize   = gridFieldSize grid
    fldPoss   = gridFieldsAbsPositions grid
    vElements = visualRepresentationGrid lvlSpec fldSize fldPoss

-- | A visual representation of the game grid, given the fields with their
-- positions, a level specification and a field size, as a list of visual elements.
visualRepresentationGrid :: LevelSpec -> Size -> [(Field, Position)] -> [VElement]
visualRepresentationGrid lvlSpec fldSize = map visualRepresentationField
  where
    visualRepresentationField (field, position) =
      (fieldColor lvlSpec (fieldId field), fldSize, position)

-- *** Visual properties of grids and fields

-- | Return for every field in a grid the position of the left upper corner
--   of the field on the screen.
gridFieldsAbsPositions :: Grid -> [(Field, ScreenPosition)]
gridFieldsAbsPositions grid = gridFieldsAbsPositions' (gridFieldsRelPositions grid)
  where
    -- | Convert relative positions (with respect to the grid) to absolute
    -- positions (with respect to the screen).
    gridFieldsAbsPositions' :: [(Field, RelScreenPosition)] -> [(Field, ScreenPosition)]
    gridFieldsAbsPositions' = fmap (\(fld, (x, y)) -> (fld, (x + screenMarginWH, y + screenMarginWH)))

-- | The relative screen positions of fields with respect to a grid. That is,
-- the field with logical position (x, y) within the grid has position
-- (x * fieldWidth + x * screenMarginWH, y * fieldHeight + y * screenMarginWH).
gridFieldsRelPositions :: Grid -> [(Field, RelScreenPosition)]
gridFieldsRelPositions grid = concatMap (fmap (\field -> (field, relPos field))) grid
 where
   (fW, fH) = gridFieldSize grid

   relPos :: Field -> RelScreenPosition
   relPos fld = (xPos * fW  + xPos * screenMarginWH, yPos * fH + yPos * screenMarginWH)
     where
       (xPos, yPos) = findPosition grid fld

-- | Calculate the field size.
-- PRE: The screen is a square ('screenWH').

-- Fields are squares.
-- Field size depends on the amount of columns and rows.
--   Use the larger one because it also needs to fit
-- Leave space for margin (left and right: 2 times).
--   Probably the margin to the right site or to the bottom is larger
--   but at least the space of 1 margin is given.
-- Leave space for margins between fields, that is:
--  (maxColRrow - 1) * fieldMargin.
gridFieldSize :: Grid -> Size
gridFieldSize grid = (fs, fs)
  where
    fs         = fieldSpace `div` maxCR
    fieldSpace = screenWH - (2 * screenMarginWH)
                          - ((maxCR - 1) * screenMarginWH)

    maxCR      = gridMaxLength grid

-- | Get the maximal size for a given grid.
--   That is, the maximum of the longest column and row.
gridMaxLength :: Grid -> Int
gridMaxLength grid = max maxColumn maxRow
  where
    maxColumn = gridLongestColumn grid
    maxRow    = gridLongestColumn $ transpose grid

    -- If you wonder where in this functions the arguments are, search for `eta
    -- conversion` and `pointfree style`.
    gridLongestColumn = maximum . map length

-- * Auxiliary Types

-- ** Visual Element

-- | A visual element with color, size and position.
type VElement = (Color, Size, Position) -- ((color, (width, height), (xPos, yPos))

-- | The color of an element.
vElementColor :: VElement -> Color
vElementColor (color, _, _) = color

-- | The size of an element.
vElementSize :: VElement -> Size
vElementSize (_, fldSize, _) = fldSize

-- | The position of an element.
vElementPosition :: VElement -> Position
vElementPosition (_, _, position) = position

-- ** Dimensions

-- | Defines a field size with (width, height).
type Size = (Int, Int)

-- | Define a position with (x, y).
type Position = (Int, Int)

-- | Define a screen position with (x, y).
type ScreenPosition = (Int, Int)

-- | Define a relative screen position with (x, y).
--
-- In this game it is used to to handle relative screen positions of fields within a grid.
type RelScreenPosition = (Int, Int)

-- * Auxiliary functions

-- | Convert 'Int' to 'Int16'.
toInt16 :: Int -> Int16
toInt16 i = fromIntegral i :: Int16

-- | Convert 'Int' to 'CInt'.
toCInt :: Int -> CInt
toCInt i = CInt (fromIntegral i :: Int32)
