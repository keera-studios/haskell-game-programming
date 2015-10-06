import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IfElse
import Data.IORef
import Debug.Trace
import FRP.Yampa as Yampa
import Text.Printf

import Game
import Display
import Input
import Graphics.UI.Extra.SDL

-- TODO: Use MaybeT or ErrorT to report errors
main :: IO ()
main = do

  initializeDisplay

  timeRef       <- initializeTimeRef
  controllerRef <- initializeInputDevices
  res           <- loadResources

  initGraphs res
  reactimate (senseInput controllerRef)
             (\_ -> do
                -- Get clock and new input
                mInput <- senseInput controllerRef
                dtSecs <- senseTime timeRef mInput
                -- trace ("Time : " ++ printf "%.5f" dtSecs) $
                return (if controllerPause mInput then 0 else dtSecs, Just mInput)
             )
             (\_ (e,c) -> do render res e
                             -- putStrLn "*********************************************"
                             return (controllerExit c)
             )
             (wholeGame &&& arr id)

type MonadicT m a b = a -> m b

senseTime :: IORef Int -> MonadicT IO Controller DTime
senseTime timeRef = \mInput ->
  let tt  = if controllerSlow mInput then (/10) else id
      tt1 = if controllerSuperSlow mInput then (/100) else tt
      tt2 = if controllerFast mInput then (*10) else tt1
  in fmap (tt2 . milisecsToSecs) $ senseTimeRef timeRef
