
import Data.IORef 
import Control.Monad
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives as SDL
import Graphics.UI.SDL.Image as SDL

main :: IO ()
main = do
  SDL.init [InitVideo]

  screen <- SDL.setVideoMode 1024 768 32 [SWSurface]
  let format = SDL.surfaceGetPixelFormat screen
  white <- SDL.mapRGB format 0xFF 0xFF 0xFF

  diffX <- newIORef 0

  forever $ do
    SDL.fillRect screen Nothing white

    filledRectangleRR screen 180 30 240 100 20 (Pixel 0xFF0000FF)
    
    rectangleRR screen 180 150 240 180 15 (Pixel 0xFF0000FF)

    filledRectangleRR screen 180 230 440 260 15 (Pixel 0x00FF00FF)
    rectangleRR       screen 180 230 440 260 15 (Pixel 0xFF0000FF)
    rectangleRR       screen 181 230 439 260 15 (Pixel 0xFF0000FF)
    rectangleRR       screen 180 231 440 259 15 (Pixel 0xFF0000FF)

    linedHLine screen 180 240 300 5 3 (Pixel 0xFF0000FF)

    dottedHLine screen 180 240 330 5 3 (Pixel 0xFF0000FF)

    SDL.flip screen

    SDL.delay 10

linedHLine surface x1 x2 y l1 l2 color =
    mapM_ (\(x1',x2') -> hLine surface x1' x2' y color) sublines
   where sublines = [ (x1 + x * segsep, x1 + x * segsep + l1)
                    | let numSegments = (x2 - x1) `div` segsep
                    , x <- [0..numSegments]
                    ]
         segsep   = l1 + l2

dottedHLine surface x1 x2 y l1 l2 color=
    mapM_ (\x -> pixel surface x y color) pixels
   where pixels   = [ (x1 + x) | x <- [0..(x2 - x1 + 1)], odd x]

rectangleRR surface x1 y1 x2 y2 rad pixel = do
    hLine surface (x1 + rad) (x2 - rad) y1 pixel
    hLine surface (x1 + rad) (x2 - rad) y2 pixel
    vLine surface x1 (y1 + rad) (y2 - rad) pixel
    vLine surface x2 (y1 + rad) (y2 - rad) pixel
    arc surface (x1 + rad) (y1 + rad) rad 180 270 pixel
    arc surface (x2 - rad) (y1 + rad) rad 270 360 pixel
    arc surface (x1 + rad) (y2 - rad) rad 090 180 pixel
    arc surface (x2 - rad) (y2 - rad) rad 000 090 pixel

filledRectangleRR surface x1 y1 x2 y2 rad pixel = do
    box surface (Rect (fI $ x1 + rad) (fI $ y1)       (fI $ x2 - rad) (fI y2))         pixel
    box surface (Rect (fI x1)         (fI $ y1 + rad) (fI x2)         (fI $ y2 - rad)) pixel
    filledPie surface (x1 + rad) (y1 + rad) rad 180 270 pixel
    filledPie surface (x2 - rad) (y1 + rad) rad 270 360 pixel
    filledPie surface (x1 + rad) (y2 - rad) rad 090 180 pixel
    filledPie surface (x2 - rad) (y2 - rad) rad 000 090 pixel
 where fI = fromIntegral
