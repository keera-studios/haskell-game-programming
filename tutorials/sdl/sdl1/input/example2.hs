import Graphics.UI.SDL            as SDL
import Graphics.UI.SDL.Primitives as SDL

data Controller = Controller
 { gunFiring :: Bool
 , gunPos    :: (Int, Int)
 }

defaultController :: Controller
defaultController = Controller False (0, 0)
-- Better to use:
-- defaultController = Controller False (width `div` 2, height `div` 2)

-- SDL controller-updating function
updateController :: Controller -> IO Controller
updateController c = do
  ev <- SDL.pollEvent
  case ev of
    SDL.NoEvent -> return (withinScreenBoundaries c)

    -- Movement
    SDL.KeyDown (Keysym SDLK_LEFT  _ _)
      -> updateController (c { gunPos = ((-1, 0) ^+^ gunPos c)})
    SDL.KeyDown (Keysym SDLK_UP    _ _)
      -> updateController (c { gunPos = ((0, -1) ^+^ gunPos c)})
    SDL.KeyDown (Keysym SDLK_DOWN  _ _)
      -> updateController (c { gunPos = ((0,  1) ^+^ gunPos c)})
    SDL.KeyDown (Keysym SDLK_RIGHT _ _)
      -> updateController (c { gunPos = ((1,  0) ^+^ gunPos c)})

    -- Fire
    SDL.KeyDown (Keysym SDLK_SPACE _ _)
      -> updateController (c { gunFiring = True})
    SDL.KeyUp   (Keysym SDLK_SPACE _ _)
      -> updateController (c { gunFiring = False})

    -- Anything else
    _ -> updateController c  -- Discard any other event

(^+^) :: Num a => (a, a) -> (a, a) -> (a, a)
(^+^) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

withinScreenBoundaries :: Controller -> Controller
withinScreenBoundaries c = c { gunPos = (x', y') }
 where x' = inRange (0, width)  (fst (gunPos c))
       y' = inRange (0, height) (snd (gunPos c))

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

  gameLoop defaultController

gameLoop :: Controller -> IO ()
gameLoop c = do
  -- Sense
  c' <- updateController c

  -- Advance game state: Nothing to do here

  -- Render
  render c'

  -- Loop
  gameLoop c'

render :: Controller -> IO ()
render controller = do
  screen <- getVideoSurface

  -- 1) Green background
  let format = SDL.surfaceGetPixelFormat screen
  green <- SDL.mapRGB format 0 0xFF 0
  SDL.fillRect screen Nothing green

  -- 2) Gun
  let (x,y) = gunPos controller
      color = if gunFiring controller
                 then Pixel 0xFF0000FF -- red  (alpha 255)
                 else Pixel 0x0000FFFF -- blue (alpha 255)
  SDL.filledCircle screen (fromIntegral x) (fromIntegral y) 30 color

  SDL.flip screen


width  = 480
height = 320
