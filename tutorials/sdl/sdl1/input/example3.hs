import Graphics.UI.SDL            as SDL
import Graphics.UI.SDL.Primitives as SDL

data Controller = Controller
 { gunFiring :: Bool
 , gunLeft   :: Bool
 , gunRight  :: Bool
 , gunUp     :: Bool
 , gunDown   :: Bool
 }

defaultController :: Controller
defaultController = Controller False False False False False

-- SDL controller-updating function
updateController :: Controller -> IO Controller
updateController c = do
  ev <- SDL.pollEvent
  case ev of
    SDL.NoEvent -> return c
    -- Movement
    SDL.KeyDown (SDL.Keysym SDLK_LEFT  _ _) -> updateController (c { gunLeft  = True  })
    SDL.KeyUp   (SDL.Keysym SDLK_LEFT  _ _) -> updateController (c { gunLeft  = False })
    SDL.KeyDown (SDL.Keysym SDLK_UP    _ _) -> updateController (c { gunUp    = True  })
    SDL.KeyUp   (SDL.Keysym SDLK_UP    _ _) -> updateController (c { gunUp    = False })
    SDL.KeyDown (SDL.Keysym SDLK_DOWN  _ _) -> updateController (c { gunDown  = True  })
    SDL.KeyUp   (SDL.Keysym SDLK_DOWN  _ _) -> updateController (c { gunDown  = False })
    SDL.KeyDown (SDL.Keysym SDLK_RIGHT _ _) -> updateController (c { gunRight = True  })
    SDL.KeyUp   (SDL.Keysym SDLK_RIGHT _ _) -> updateController (c { gunRight = False })

    -- Fire
    SDL.KeyDown (SDL.Keysym SDLK_SPACE _ _) -> updateController (c { gunFiring = True})
    SDL.KeyUp   (SDL.Keysym SDLK_SPACE _ _) -> updateController (c { gunFiring = False})

    -- Anything else
    _ ->
      updateController c  -- Discard any other event

data GameState = GameState
  { gsGamePos  :: (Int, Int)
  , gsGameFire :: Bool
  }

initialGameState :: GameState
initialGameState = GameState
  { gsGamePos  = (0, 0)
  , gsGameFire = False
  }


(^+^) :: Num a => (a, a) -> (a, a) -> (a, a)
(^+^) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

updateGameLogic :: Controller -> GameState -> GameState
updateGameLogic c gs = gs'
 where gs' = gs { gsGameFire = gunFiring c
                , gsGamePos  = withinScreenBoundaries (vtotal ^+^ gsGamePos gs)
                }

       -- Displacement caused by input controller state
       vtotal = vl ^+^ vr ^+^ vu ^+^ vd

       -- Displacement caused by controller in each direction
       vl = if gunLeft  c then (-1, 0) else (0, 0)
       vr = if gunRight c then (1,  0) else (0, 0)
       vu = if gunUp    c then (0, -1) else (0, 0)
       vd = if gunDown  c then (0,  1) else (0, 0)

-- Write this for the game state instead
withinScreenBoundaries :: (Int, Int) -> (Int, Int)
withinScreenBoundaries (x, y) = (x', y')
 where x' = inRange (0, width) x
       y' = inRange (0, height) y

inRange :: Ord a => (a,a) -> a -> a
inRange (mn, mx) v
 | v < mn    = mn
 | v > mx    = mx
 | otherwise = v

main :: IO ()
main = do
  SDL.init [InitVideo]
  SDL.setVideoMode width height 32 [SWSurface]
  SDL.setCaption "Input test" ""

  gameLoop defaultController initialGameState

gameLoop :: Controller -> GameState -> IO ()
gameLoop c gs = do
  -- Sense
  c' <- updateController c

  -- Advance game state: Nothing to do here
  let gs' = updateGameLogic c' gs

  -- Render
  render gs'

  -- Loop
  gameLoop c' gs'


render :: GameState -> IO ()
render gs = do
  screen <- getVideoSurface

  -- 1) Green background
  let format = SDL.surfaceGetPixelFormat screen
  green <- SDL.mapRGB format 0 0xFF 0
  SDL.fillRect screen Nothing green

  -- 2) Gun
  let (x,y) = gsGamePos gs
      color = if gsGameFire gs
                 then Pixel 0xFF0000FF -- red  (alpha 255)
                 else Pixel 0x0000FFFF -- blue (alpha 255)
  SDL.filledCircle screen (fromIntegral x) (fromIntegral y) 30 color

  SDL.flip screen

  SDL.delay 10


width  = 480
height = 320
