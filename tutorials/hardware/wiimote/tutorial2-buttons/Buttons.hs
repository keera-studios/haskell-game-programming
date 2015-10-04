import Control.Concurrent
import Control.Monad
import System.CWiid
import Graphics.UI.SDL as SDL
import Data.IORef
import System.IO

main :: IO ()
main = do
  -- SDL.init []
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

      -- t <- SDL.getTicks

      -- fpsCounter <- newIORef (0, t)

      -- "game" loop
      forever $ do

        -- Input: Poll new state
        allButtons <- cwiidGetBtnState wiimote
        let btnAPushed = cwiidIsBtnPushed allButtons cwiidBtnA

        -- Rendering: Report
        let msg = if btnAPushed then "#" else "."
        putStr msg
        hFlush stdout

        -- t' <- SDL.getTicks
        -- (n,t) <- readIORef fpsCounter
        -- let td  = t' - t
        -- let tpf = fromIntegral td / fromIntegral n / 1000

        -- if td > 1000
        --   then do putStrLn $ "Time per frame (in seconds): " ++ show tpf
        --           putStrLn $ "FPS: " ++ show (1.0 / tpf)
        --           writeIORef fpsCounter (0, t')
        --   else writeIORef fpsCounter (n + 1, t)

        -- Introduce a small delay
        -- threadDelay 10
