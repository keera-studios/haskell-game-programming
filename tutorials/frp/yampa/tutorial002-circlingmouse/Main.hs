{-# LANGUAGE Arrows #-}
import Data.IORef
import FRP.Yampa                  as Yampa
import Graphics.UI.SDL            as SDL
import Graphics.UI.SDL.Primitives as SDL

main = do
  -- Initialise SDL
  SDL.init [InitVideo]
  setVideoMode width height 16 [HWSurface, DoubleBuf]

  timeRef <- newIORef (0 :: Int)
  reactimate (getMousePos)
             (\_ -> do
                dtSecs <- yampaSDLTimeSense timeRef
                (x,y)  <- getMousePos
                return (dtSecs, Just (x,y)))
             (\_ e -> display e >> return False)
             inCircles

-- Pure SF
inCircles :: SF (Double, Double) (Double, Double)
inCircles = proc (centerX, centerY) -> do
  t <- time -< ()
  let x      = centerX + cos t * radius
      y      = centerY + sin t * radius
      radius = 30
  returnA -< (x,y)

-- Input
getMousePos :: IO (Double, Double)
getMousePos = do
  pumpEvents
  (x,y,_) <- SDL.getMouseState
  return (fromIntegral x, fromIntegral y)

-- Output
display :: (Double, Double) -> IO()
display (x, y) = do 
  -- Obtain surface
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  green <- mapRGB format 0 0xFF 0
  fillRect screen Nothing green

  let side = 10
  -- Paint small red square, at an angle 'angle' with respect to the center
      x'   = round x
      y'   = round y
  filledCircle screen x' y' side (Pixel 0xFF0000FF)

  -- Double buffering
  SDL.flip screen

-- Clock

-- | Updates the time in an IO Ref and returns the time difference
updateTime :: IORef Int -> Int -> IO Int
updateTime timeRef newTime = do
  previousTime <- readIORef timeRef
  writeIORef timeRef newTime
  return (newTime - previousTime)

yampaSDLTimeSense :: IORef Int -> IO Yampa.DTime
yampaSDLTimeSense timeRef = do
  -- Get time passed since SDL init
  newTime <- fmap fromIntegral SDL.getTicks

  -- Obtain time difference
  dt <- updateTime timeRef newTime
  let dtSecs = fromIntegral dt / 1000
  return dtSecs

width :: Num a => a
width  = 640

height :: Num a => a
height = 480
