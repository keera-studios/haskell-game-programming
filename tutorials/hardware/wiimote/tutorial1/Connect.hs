import System.CWiid

main :: IO ()
main = do
  putStrLn "Initializing WiiMote. Please press 1+2 to connect."
  wm <- cwiidOpen
  case wm of
    Just _aWiimote -> putStrLn "Connected"
    Nothing        -> putStrLn "Could not connect"
