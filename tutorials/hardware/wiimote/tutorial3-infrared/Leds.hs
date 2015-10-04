import Control.Concurrent
import Control.Monad
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives as SDL
import System.CWiid

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

      -- Enable different sensors (15 = 1 + 2 + 4 + 8):
      -- 1 for status, 2 for buttons, 4 for accelerometers, and 8 for IR.
      cwiidSetRptMode wiimote 15

      -- "game" loop
      forever $ do
        irs   <- cwiidGetIR wiimote
        
        -- -- Obtain positions of leds 1 and 2 (with a normal wii bar, those
        -- -- will be the ones we use).
        -- let led1   = irs!!0
        --     led2   = irs!!1
        -- 
        -- -- Calculate mid point between sensor bar leds
        -- let posX = ((cwiidIRSrcPosX led1) + (cwiidIRSrcPosX led2)) `div` 2
        --     posY = ((cwiidIRSrcPosY led1) + (cwiidIRSrcPosY led2)) `div` 2
        -- 
        -- -- Calculate proportional coordinates
        -- let propX = fromIntegral (1024 - posX) / 1024.0
        --     propY = fromIntegral (max 0 (posY - 384)) / 384.0
        -- 
        -- -- Calculate game area coordinates
        -- let finX  = width  * propX
        --     finY  = height * propY
        -- 
        -- -- Clicks
        -- let isClick = cwiidIsBtnPushed flags cwiidBtnA
        
        -- Render
        let format = surfaceGetPixelFormat screen
        white <- mapRGB format 255 255 255
        fillRect screen Nothing white

        forM_ irs $ \ir -> do
          let x = fromIntegral $ cwiidIRSrcPosX ir
              y = fromIntegral $ cwiidIRSrcPosY ir
          circle screen x y 30 (Pixel 0x0000FFFF)    

        SDL.flip screen
 
width = 1024
height = 768
