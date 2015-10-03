import Control.Concurrent
import Control.Monad
import System.CWiid

main :: IO ()
main = do
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

        -- Input: Poll new state
        allButtons <- cwiidGetBtnState wiimote
        let btnAPushed = cwiidIsBtnPushed allButtons cwiidBtnA

        -- Rendering: Report
        let msg = if btnAPushed then "Down" else "Up"
        putStrLn msg

        -- Introduce a small delay
        threadDelay 100000

