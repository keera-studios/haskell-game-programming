module Debug where

import Control.Monad (when, void)

import Constants
-- import Android.Log

debugTag = "uk.co.keera.games.escape"

debug :: Bool -> String -> IO ()
-- debug b msg = when b $ void $
--  androidLogPrint AndroidLogPrioVerbose debugTag msg
debug b msg = when b $ void $
 putStrLn $ debugTag ++ " :: " ++ msg
