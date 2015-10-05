import Control.Monad
import Graphics.UI.SDL            as SDL
import Graphics.UI.SDL.Primitives as SDL

main :: IO ()
main = do
  SDL.init [InitVideo]
  screen <- setVideoMode width height 32 [SWSurface]

  render 0

render :: Int -> IO ()
render percent = do

  -- Clear screen
  screen <- SDL.getVideoSurface
  let format = SDL.surfaceGetPixelFormat screen
  green <- SDL.mapRGB format 0 0xFF 0
  SDL.fillRect screen Nothing green

  -- Calculate new pos
  let angle = 2 * pi * (fromIntegral percent) / 100
      x = round (width  / 2 + cos angle * radius)
      y = round (height / 2 + sin angle * radius)
      radius = 50 -- this is the rotation
                  -- radius, not the circle's

  -- Draw circle
  SDL.filledCircle screen x y 30 (Pixel 0xFF0000FF) -- red

  SDL.flip screen

  if percent > 100
     then render 0
     else render (percent + 1)

width :: Num a => a
width = 480

height :: Num a => a
height = 320
