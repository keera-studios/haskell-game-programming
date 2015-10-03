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

      tl <- calibrate wiimote 10           10            -- TopLeft
      tr <- calibrate wiimote (width - 40) 10            -- TopRight
      br <- calibrate wiimote (width - 40) (height - 40) -- BottomRight
      bl <- calibrate wiimote 10           (height - 40) -- BottomLeft

      let matrix = calculateMatrix tl tr br bl

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

        let transX = applyMatrix propX -- (or maybe posX and posY)
            transY = applyMatrix propY
        
        -- Calculate game area coordinates
        let finX  = width  * transX 
            finY  = height * transY
        
        -- Clicks
        let isClick = cwiidIsBtnPushed flags cwiidBtnA
        
        -- Render
        let format = surfaceGetPixelFormat screen
        white <- mapRGB format 255 255 255
        fillRect screen Nothing white

        when (length irs > 1) $ void $ do
          let color = if isClick then Pixel 0xFF0000FF
                                 else Pixel 0x0000FFFF
              -- Type transformation Int -> Int16
          let x = round finX
              y = round finY
          circle screen x y 30 (Pixel 0x0000FFFF)    

        SDL.flip screen
 
width :: Num a => a
width = 1024

height :: Num a => a
height = 768

calibrate wiimote x y = do
  flags <- cwiidGetBtnState wiimote
  pt    <- getIRMidPoint wiimote

  -- Clicks
  let isClick = cwiidIsBtnPushed flags cwiidBtnA

  if isClick
    then return pt
    else do
      drawCross x y
      delay 10
      calibrate wiimote x y

drawCross x y = do

  screen <- getVideoSurface

  -- Render
  let format = surfaceGetPixelFormat screen
  white <- mapRGB format 255 255 255
  fillRect screen Nothing white

  hLine screen x (y + 15) 30 (Pixel 0x333333FF)
  vLine screen (x+15) y   30 (Pixel 0x333333FF)

  SDL.flip screen

getIRMidPoint wiimote = do
        irs   <- cwiidGetIR wiimote

        -- Obtain positions of leds 1 and 2 (with a normal wii bar, those
        -- will be the ones we use).
        let led1 = irs!!0
            led2 = irs!!1
        
        -- Calculate mid point between sensor bar leds
        let posX = ((cwiidIRSrcPosX led1) + (cwiidIRSrcPosX led2)) `div` 2
            posY = ((cwiidIRSrcPosY led1) + (cwiidIRSrcPosY led2)) `div` 2
        
        return (posX, posY)

calculateMatrix = undefined
applyMatrix     = undefined
