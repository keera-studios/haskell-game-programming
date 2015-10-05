import Graphics.UI.SDL            as SDL
import Graphics.UI.SDL.Primitives as SDL

data Controller = Controller
 { gunFiring :: Bool
 , gunPos    :: (Int, Int)
 }

defaultController :: Controller
defaultController = Controller False (0,0)

-- SDL controller-updating function
updateController :: Controller -> IO Controller
updateController c = do
  ev <- SDL.pollEvent
  case ev of
    -- Mouse interaction
    SDL.MouseButtonUp _ _ SDL.ButtonLeft
      -> updateController (c { gunFiring = False })
    SDL.MouseButtonDown _ _ SDL.ButtonLeft
      -> updateController (c { gunFiring = True  })
    SDL.MouseMotion x y _ _
       -> updateController (c { gunPos = (fromIntegral x, fromIntegral y) })

    -- End of queue or any other event
    SDL.NoEvent -> return c
    _           -> updateController c  -- Discard other events

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
