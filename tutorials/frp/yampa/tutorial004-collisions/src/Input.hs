-- | Defines an abstraction for the game controller and the functions to read
-- it.
--
-- Lower-level devices replicate the higher-level API, and should accomodate to
-- it. Each device should:
--
--    - Upon initialisation, return any necessary information to poll it again.
--
--    - Update the controller with its own values upon sensing.
--
-- In this case, we only have one:  mouse/keyboard combination.
--
module Input where

-- External imports
import Data.IORef
import Graphics.UI.SDL as SDL

-- Internal imports
import Control.Extra.Monad
import Graphics.UI.Extra.SDL

-- * Game controller

-- | Controller info at any given point.
data Controller = Controller
  { controllerPos        :: (Double, Double)
  , controllerClick      :: Bool
  , controllerPause      :: Bool
  , controllerExit       :: Bool
  , controllerFast       :: Bool
  , controllerSlow       :: Bool
  , controllerSuperSlow  :: Bool
  , controllerFullscreen :: Bool
  }

-- | Controller info at any given point, plus a pointer
-- to poll the main device again. This is safe,
-- since there is only one writer at a time (the device itself).
newtype ControllerRef =
  ControllerRef { controllerData :: (IORef Controller, Controller -> IO Controller) }

-- * General API

-- | Initialize the available input devices. This operation
-- returns a reference to a controller, which enables
-- getting its state as many times as necessary. It does
-- not provide any information about its nature, abilities, etc.
initializeInputDevices :: IO ControllerRef
initializeInputDevices = do
  nr <- newIORef defaultInfo
  return $ ControllerRef (nr, sdlGetController)
 where defaultInfo = Controller (0,0) False False False False False False False

-- | Sense from the controller, providing its current
-- state. This should return a new Controller state
-- if available, or the last one there was.
-- 
-- It is assumed that the sensing function is always
-- callable, and that it knows how to update the
-- Controller info if necessary.
senseInput :: ControllerRef -> IO Controller
senseInput (ControllerRef (cref, sensor)) =
  modifyIORefIO cref sensor

type ControllerDev = IO (Maybe (Controller -> IO Controller))

-- * SDL API (mid-level)

-- ** Sensing

-- | Sense the SDL keyboard and mouse and update
-- the controller. It only senses the mouse position,
-- the primary mouse button, and the p key to pause
-- the game.
--
-- We need a non-blocking controller-polling function.
-- TODO: Check http://gameprogrammer.com/fastevents/fastevents1.html
sdlGetController :: Controller -> IO Controller
sdlGetController info =
  foldLoopM info pollEvent (not.isEmptyEvent) ((return .) . handleEvent)

handleEvent :: Controller -> SDL.Event -> Controller
handleEvent c e =
  case e of
    MouseMotion x y _ _                      -> c { controllerPos        = (fromIntegral x, fromIntegral y)}
    MouseButtonDown _ _ ButtonLeft           -> c { controllerClick      = True }
    MouseButtonUp   _ _ ButtonLeft           -> c { controllerClick      = False}
    KeyUp (Keysym { symKey = SDLK_p })       -> c { controllerPause      = not (controllerPause c) }
    KeyUp (Keysym { symKey = SDLK_f })       -> c { controllerFullscreen = not (controllerFullscreen c) }
    KeyDown (Keysym { symKey = SDLK_w })     -> c { controllerSuperSlow  = True  }
    KeyUp   (Keysym { symKey = SDLK_w })     -> c { controllerSuperSlow  = False }
    KeyDown (Keysym { symKey = SDLK_s })     -> c { controllerSlow       = True  }
    KeyUp   (Keysym { symKey = SDLK_s })     -> c { controllerSlow       = False }
    KeyDown (Keysym { symKey = SDLK_x })     -> c { controllerFast       = True  }
    KeyUp   (Keysym { symKey = SDLK_x })     -> c { controllerFast       = False }
    KeyDown (Keysym { symKey = SDLK_SPACE }) -> c { controllerClick      = True  }
    KeyUp (Keysym { symKey = SDLK_SPACE })   -> c { controllerClick      = False }
    KeyDown (Keysym { symKey = SDLK_ESCAPE}) -> c { controllerExit       = True  }
    _                                        -> c


-- * Aux IOREf
modifyIORefIO :: IORef a -> (a -> IO a) -> IO a
modifyIORefIO ref modify = do
  v <- readIORef ref
  new <- modify v
  writeIORef ref new
  return new
