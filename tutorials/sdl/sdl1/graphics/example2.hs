import Control.Monad         (forever)
import Graphics.UI.SDL       as SDL
import Graphics.UI.SDL.Image as SDL

main :: IO ()
main = do
  -- Initialization
  SDL.init [InitVideo]

  -- NEW: load image
  image <- SDL.load "assets/bunny1_jump.png"
  let imgWidth  = surfaceGetWidth  image
      imgHeight = surfaceGetHeight image

  -- Configuration
  screen <- SDL.setVideoMode 480 320 32 [SWSurface]
  SDL.setCaption "Test" ""

  forever $ do
    -- You can also do this here to get the main video surface
    -- screen <- SDL.getVideoSurface

    let format = SDL.surfaceGetPixelFormat screen
    green <- SDL.mapRGB format 0xAA 0xFF 0xAA
    SDL.fillRect screen Nothing green

    -- NEW: Paste the image surface onto the gree background
    SDL.blitSurface image Nothing screen (Just (Rect 30 30 imgWidth imgHeight))

    -- Double buffering
    SDL.flip screen
