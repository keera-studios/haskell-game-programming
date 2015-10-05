import Control.Monad   (forever)
import Graphics.UI.SDL as SDL

main :: IO ()
main = do
  SDL.init [InitVideo]
  screen <- SDL.setVideoMode 480 320 32 [SWSurface]
  forever $ do
    let format = surfaceGetPixelFormat screen
    green <- SDL.mapRGB format 0 0xFF 0
    SDL.fillRect screen Nothing green
    SDL.flip screen

    -- NEW
    n <- SDL.getTicks
    print n
