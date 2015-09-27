
import Data.IORef 
import Control.Monad
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives as SDL
import Graphics.UI.SDL.Image as SDL

main :: IO ()
main = do
  SDL.init [InitVideo]

  gioconda <- SDL.load "tutorials/sdl/sdl1/drawings/gioconda.jpg"

  screen <- SDL.setVideoMode 1024 768 32 [SWSurface]
  let format = SDL.surfaceGetPixelFormat screen
  white <- SDL.mapRGB format 0xFF 0xFF 0xFF

  diffX <- newIORef 0

  forever $ do
    SDL.fillRect screen Nothing white

    -- Thick horizontal line (x1 x2 y)
    hLine screen 12 100 30 (Pixel 0x00FF00FF)
    hLine screen 12 100 31 (Pixel 0x00FF00FF)
    hLine screen 12 100 32 (Pixel 0x00FF00FF)

    -- Thick horizontal line (x y1 y2)
    vLine screen 140 30 100 (Pixel 0x00FF00FF)
    vLine screen 141 30 100 (Pixel 0x00FF00FF)
    vLine screen 142 30 100 (Pixel 0x00FF00FF)

    -- Rectangle (Rect x1 y1 x2 y2)
    rectangle screen (Rect 180 30 240 100) (Pixel 0xFF0000FF)
    rectangle screen (Rect 185 35 245 105) (Pixel 0xFF0000FF)
    rectangle screen (Rect 190 40 250 110) (Pixel 0xFF0000FF)

    -- Circle (x y radius)
    circle screen 320 65 30 (Pixel 0x0000FFFF)    
    circle screen 320 65 31 (Pixel 0x0000FFFF)    
    circle screen 320 65 32 (Pixel 0x0000FFFF)    

    -- Box 
    box screen (Rect 400 40 500 100) (Pixel 0x0000FFFF)

    -- Line
    line screen 540 40 590 100 (Pixel 0xFF00FFFF)
    line screen 541 40 591 100 (Pixel 0xFF00FFFF)
    line screen 542 40 592 100 (Pixel 0xFF00FFFF)

    -- Line
    aaLine screen 610 40 660 100 (Pixel 0xFF00FFFF)
    aaLine screen 611 40 661 100 (Pixel 0xFF00FFFF)
    aaLine screen 612 40 662 100 (Pixel 0xFF00FFFF)

    -- Arc (x y rad start_angle end_angle -- goes clockwise)
    arc screen 100 200 30 0 100 (Pixel 0xAA0000FF)

    -- Anti-aliased circle
    aaCircle screen 220 200 30 (Pixel 0x0000FFFF)    

    filledCircle screen 320 200 30 (Pixel 0x0000FFFF)    

    -- Ellipse (x y h_rad v_rad)
    ellipse screen 400 200 10 30 (Pixel 0x990099FF)

    aaEllipse screen 460 200 10 30 (Pixel 0x990099FF)

    filledEllipse screen 520 200 10 30 (Pixel 0x990099FF)

    -- Pie
    pie screen 600 200 30 0 100 (Pixel 0xAA0000FF)

    filledPie screen 600 200 30 0 100 (Pixel 0xAA0000FF)

    -- Trigon
    trigon screen 100 300 150 300 125 450 (Pixel 0x227744FF)

    aaTrigon screen 200 300 250 300 225 450 (Pixel 0x227744FF)
    
    filledTrigon screen 300 300 350 300 325 450 (Pixel 0x227744FF)

    -- Polygon
    polygon screen [(100, 500), (150, 500), (170, 520), (150, 560)] (Pixel 0xAA00AAFF)

    aaPolygon screen [(200, 500), (250, 500), (270, 520), (250, 560)] (Pixel 0xAA00AAFF)

    filledPolygon screen [(300, 500), (350, 500), (370, 520), (350, 560)] (Pixel 0xAA00AAFF)

    x <- readIORef diffX
    texturedPolygon screen [(400, 500), (1000, 500), (1000, 700), (430, 680)] gioconda (0 + x `div` 2) 0
    writeIORef diffX ((x + 1) `mod` 600)

    -- Beizer
    --
    SDL.flip screen

    SDL.delay 10    
