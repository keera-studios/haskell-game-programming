import Control.Monad   (forever)
import Graphics.UI.SDL as SDL

main :: IO ()
main = do
  SDL.init [InitVideo]
  _ <- SDL.setVideoMode 480 320 32 [SWSurface]

  -- We need the start time, so that the first
  -- report is reliable (in case setting up SDL
  -- takes too long)
  lastTime <- getTicks
  render (fromIntegral lastTime) 0

render :: Int -> Int -> IO ()
render lastTime numFrames = do
    -- Do something expensive, like drawing on the screen
    screen <- SDL.getVideoSurface
    let format = surfaceGetPixelFormat screen
    green <- SDL.mapRGB format 0 0xFF 0
    SDL.fillRect screen Nothing green
    SDL.flip screen

    -- NEW: report FPS every second
    newTime <- SDL.getTicks
    let newTime' = fromIntegral newTime

    let timeNextReport = lastTime + 1000         -- print one report per second
        timeDiff       = newTime' - lastTime     -- time passed since last report

        -- Time passed, in seconds
        timeDiffSecs :: Float
        timeDiffSecs = fromIntegral timeDiff / 1000

        -- Num frames in time passed
        fps :: Float
        fps = fromIntegral numFrames / timeDiffSecs

    -- Print report and start over,
    -- or continue looping and increase number of frames
    if newTime' > timeNextReport
      then do print fps
              render newTime' 0
      else render lastTime (numFrames + 1)
