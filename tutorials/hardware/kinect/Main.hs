import Freenect
import Data.Vector.Storable as V hiding ((++), length, foldr)
import qualified Data.Vector.Storable as V
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives as SDL
import Data.IORef
import Data.Word
import Data.Maybe

-- TODO Use these instead of hard-coded values
kinectWidth, kinectHeight :: Int
kinectWidth  = 640
kinectHeight = 480

type KinectPosRef = IORef KinectPos
type KinectPos = Maybe (Double, Double)

initializeKinect :: (Double, Double) -> IO KinectPosRef
initializeKinect screenSize = do
  lastPos <- newIORef Nothing
  _ <- getDepthThread screenSize lastPos
  return lastPos

getDepthThread :: (Double, Double) -> KinectPosRef -> IO ThreadId
getDepthThread screenSize lastPos = forkIO $ do
  withContext $ \context -> do
    setLogLevel LogFatal context
    selectSubdevices context devices
    withDevice context index $ \device -> do
      setDepthMode device Medium ElevenBit
      setDepthCallback device $ \payload _timestamp -> do
        maybe (print ".") -- Too far or too close
              (updatePos lastPos)
              (calculateMousePos screenSize payload)
        return ()
      startDepth device
      forever $ processEvents context

  where devices = [Camera]
        index = 0 :: Integer

updatePos :: IORef (Maybe (Double, Double)) -> (Double, Double) -> IO ()
updatePos lastPosRef newPos@(nx,ny) = do
  lastPosM <- readIORef lastPosRef
  let (mx, my) = case lastPosM of
                   Nothing        -> newPos
                   (Just (lx,ly)) -> (adjust 50 lx nx, adjust 50 ly ny)
  writeIORef lastPosRef (Just (mx, my))
  mx `seq` my `seq` return ()

calculateMousePos :: (Double, Double) -> Vector Word16 -> Maybe (Double, Double) 
calculateMousePos (width, height) payload =
  fmap g (findFirst payload)
  where g (px,py) = (mousex, mousey)
         where
           pointerx = fromIntegral (640 - px)
           pointery = fromIntegral py
           mousex   = pointerx * adjx
           mousey   = pointery * adjy
           adjx     = width  / 630.0
           adjy     = height / 470.0

mat :: Vector Float
mat = V.generate 2048 (\i -> let v :: Float
                                 v = ((fromIntegral i/2048.0)^3)*6.0 in v * 6.0 * 256.0)

findFirst :: Vector Word16 -> Maybe (Int, Int)
findFirst vs = fmap (\v -> (v `mod` 640, v `div` 640)) i
 where i  = V.findIndex (\x -> mat!(fromIntegral x) < 512) vs

processPayload :: Vector Word16 -> [(Float, Int, Int)]
processPayload ps = [(pval, tx, ty) | i <- [0..640*480-1]
                                    , let pval = mat!(fromIntegral (ps!i))
                                    , pval < 300
                                    , let ty = i `div` 640
                                          tx = i `mod` 640
                                    ]

-- Drop the fst elem, calculate the avg of snd and trd over the whole list
avg :: [(Float, Int, Int)] -> (Int, Int)
avg ls = (sumx `div` l, sumy `div` l)
  where l = length ls
        (sumx, sumy) = foldr (\(_,x,y) (rx,ry) -> (x+rx,y+ry)) (0,0) ls

-- Update a value, with a max cap
adjust :: (Num a, Ord a) => a -> a -> a -> a
adjust maxD old new
  | abs (old - new) < maxD = new
  | old < new              = old + maxD
  | otherwise              = old - maxD

main :: IO ()
main = do
  SDL.init [InitVideo]

  screen <- SDL.setVideoMode width height 32 [SWSurface]

  putStrLn "Initializing Kinect."

  kinectPos <- initializeKinect (width, height)

  -- NEW
  t <- SDL.getTicks
  fpsCounter <- newIORef (0, t)

  -- "game" loop
  forever $ do
    
    -- Render
    let format = surfaceGetPixelFormat screen
    white <- mapRGB format 255 255 255
    fillRect screen Nothing white

    t' <- SDL.getTicks
    (n,t) <- readIORef fpsCounter
    let td  = t' - t
    let tpf = fromIntegral td / fromIntegral n / 1000

    if td > 1000
      then do putStrLn $ "Time per frame (in seconds): " ++ show tpf
              putStrLn $ "FPS: " ++ show (1.0 / tpf)
              writeIORef fpsCounter (0, t')
      else writeIORef fpsCounter (n + 1, t)

    (x,y) <- fromMaybe (width / 2, height / 2) <$> readIORef kinectPos

    let color = Pixel 0xFF0000FF
        x'    = round x
        y'    = round y
    filledCircle screen x' y' 30 color

    SDL.flip screen
 
width :: Num a => a
width = 1024

height :: Num a => a
height = 768

