import Control.Concurrent
import Control.Monad
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives as SDL
import System.CWiid
import Data.IORef

main :: IO ()
main = do
  SDL.init [InitVideo]

  screen <- SDL.setVideoMode width height 32 [SWSurface]

  putStrLn "Initializing WiiMote. Please press 1+2 to connect."
  wm <- cwiidOpen
  case wm of
    Nothing      -> putStrLn "Could not connect"
    Just wiimote -> do 
      putStrLn "Connected"

      -- NEW
      t <- SDL.getTicks
      fpsCounter <- newIORef (0, t)

      -- Enable different sensors (15 = 1 + 2 + 4 + 8):
      -- 1 for status, 2 for buttons, 4 for accelerometers, and 8 for IR.
      cwiidSetRptMode wiimote 15

      -- "game" loop
      forever $ do
        flags <- cwiidGetBtnState wiimote
        irs   <- cwiidGetIR wiimote
        
        -- Obtain positions of leds 1 and 2 (with a normal wii bar, those
        -- will be the ones we use).
        let led1 = irs!!0
            led2 = irs!!1
        
        -- Calculate mid point between sensor bar leds
        let posX = ((cwiidIRSrcPosX led1) + (cwiidIRSrcPosX led2)) `div` 2
            posY = ((cwiidIRSrcPosY led1) + (cwiidIRSrcPosY led2)) `div` 2
        
        -- Calculate proportional coordinates
        let propX = fromIntegral (1024 - posX) / 1024.0
            propY = fromIntegral (max 0 (posY - 384)) / 384.0
        
        -- Calculate game area coordinates
        let finX  = width  * propX
            finY  = height * propY
        
        -- Clicks
        let isClick = cwiidIsBtnPushed flags cwiidBtnA
        
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


        when (length irs > 1) $ void $ do
          let color = if isClick then Pixel 0xFF0000FF
                                 else Pixel 0x0000FFFF
              -- Type transformation Int -> Int16
          let x = round finX
              y = round finY
          filledCircle screen x y 30 color

        SDL.flip screen
 
width :: Num a => a
width = 1024

height :: Num a => a
height = 768
