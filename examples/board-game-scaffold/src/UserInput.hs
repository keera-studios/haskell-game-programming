-- | Defines the handling of user mouse input in a game controller using SDL2.
--
-- The module contains two parts.  First, a definition of the game controller
-- containing informations needed to handle the user-game interaction.
--
-- Second, the sensing of the needed informations using mouse events of SDL2.
module UserInput where

-- External imports
import Data.Int (Int32)
import SDL      (EventPayload (MouseButtonEvent, MouseMotionEvent),
                 InputMotion (Pressed, Released), Point (P), V2 (V2),
                 eventPayload, mouseButtonEventMotion, mouseMotionEventPos,
                 pollEvent)

-- Internal imports

-- * Mouse controller.

-- | Controller information.
data Controller = Controller
  { controllerPos      :: (Int, Int)               -- ^ Position of the given controller.
  , controllerClick    :: Bool                     -- ^ Controller received click notification.
  , controllerClickPos :: (Int, Int)               -- ^ Controller slide start position.
  , controllerDragPoss :: ((Int, Int), (Int, Int)) -- ^ The start and end position of a controller drag.
  } deriving (Eq)

-- | Default controller.
defaultController :: Controller
defaultController = Controller (0,0) False (0,0) ((0,0), (0,0))

-- * Sensing

-- | SDL2 controller updating function.
updateController :: Controller -> IO Controller
updateController ctrl = do
  event <- pollEvent
  case event of
    Nothing     -> return ctrl
    Just event' -> updateController (processEvent ctrl (eventPayload event'))

-- | Processes an event by updating the controller.

-- Uses pattern guards.
processEvent :: Controller -> EventPayload -> Controller
processEvent ctrl event
  -- Mouse button released
  | MouseButtonEvent ev <- event
  , Released            <- mouseButtonEventMotion ev
  = ctrl { controllerClick = False
        , controllerDragPoss = (controllerClickPos ctrl, controllerPos ctrl)
        }

  -- Mouse button pressed
  | MouseButtonEvent ev <- event
  , Pressed             <- mouseButtonEventMotion ev
  = ctrl { controllerClick = True
        , controllerClickPos = controllerPos ctrl
        }

  -- Mouse motion
  | MouseMotionEvent ev <- event
  , pos                 <- mouseMotionEventPos ev
  = ctrl { controllerPos = pointToPair pos }

  -- Discard other events
  | otherwise
  = ctrl

-- * Auxiliary functions

-- | Convert mouse position.
pointToPair :: Point V2 Int32 -> (Int, Int)
pointToPair (P (V2 x y)) = (fromIntegral x, fromIntegral y)
