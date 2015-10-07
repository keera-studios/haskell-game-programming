{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Extra.Monad
import Data.IORef
import FRP.Yampa                  as Yampa
import FRP.Yampa.Extensions       as Yampa
import FRP.Yampa.Backends.SDL     as Yampa
import FRP.Yampa.Reactimator      as Yampa
import Graphics.UI.SDL            as SDL
import Graphics.UI.SDL.Extensions as SDL
import System.Random

main :: IO ()
main = do

  -- Sources of randomness, used for the top and bottom bars
  stdGen1 <- newStdGen
  stdGen2 <- getStdGen

  -- Input sensing, time sensing, rendering
  (producer, initSample) <- initializeSg :: IO (SDLSignal SDLInput, Controller)
  consumer               <- initializeSi :: IO MySDLRenderer

  -- Reactimation
  alwaysReactimate (return initSample)
                   (pollSg producer)
                   (pushSi consumer)
                   (game stdGen1 stdGen2)

-- * Game

game :: StdGen -> StdGen -> SF Controller (Int, ([Int], [Int]))
game stdGen1 stdGen2 = proc (c) -> do
  -- (identity &&& allBars) >>> (bird &&& arr snd)
  sBars <- allBars -< ()
  sBird <- bird    -< (c, sBars)
  returnA -< (sBird, sBars)
 where -- Top bars move slower, bottom bars move faster
       allBars = bars stdGen1 &&& timeTransform (*3) (bars stdGen2)

-- Restart every time the bird hits one of the bars
bird :: SF (Controller, ([Int],[Int])) Int
bird = switch
  ((birdWithinBounds *** identity) >>> (arr fst &&& birdHitBars))
  (\_ -> bird)
 where birdHitBars = arr birdOnBars >>> edge
       birdOnBars (playerY, (barsTop, barsBottom)) =
         any (> playerY) selectedTops || any (< playerY) selectedBottoms
        where bottomBarTops   = map (\x -> height - x - playerHeight) barsBottom
              selectedTops    = take playerWidth $ drop playerX barsTop
              selectedBottoms = take playerWidth $ drop playerX bottomBarTops

-- Restart every time the bird hits the boundaires
birdWithinBounds :: SF Controller Int
birdWithinBounds = switch
   (aliveBird >>> (identity &&& hitBounds))
   (\_ -> birdWithinBounds) -- Restart from middle of the screen
 where hitBounds   = arr outOfBounds >>> edge
       outOfBounds = ((> height - playerHeight) &&& (< 0)) >>> uncurry (||)

-- Just a falling/rising bird
aliveBird :: SF Controller Int
aliveBird = aliveBird0 (height / 2 :: Double)

aliveBird0 :: Double -> SF Controller Int
aliveBird0 p0 = proc (c) -> do
  -- Acceleration up/down depending on input
  let acc = if controllerClick c then -stdAcc else stdAcc
  -- Otherwise, obey basic physics laws
  v <- integral -< acc
  p <- (p0 +) ^<< integral -< v
  returnA -< round p

bars :: RandomGen g => g -> SF a [Int]
bars g = barSides >>> hold 0 >>> sampleWindow' 0 width 0.1 >>> hold []
 where -- Bunch of events carrying random altitudes (at regular times)
       barSides :: SF a (Yampa.Event Int)
       barSides = (barsUp `rMergeSF` barsDown)

       -- Bunch of events, carrying random values (bar heights)
       barsUp :: SF a (Yampa.Event Int)
       barsUp = (occasionalEvent &&& justNoise) >>> arr (uncurry tag)

       -- Bunch of events, carrying zero, at regular times, with a bar width delay
       barsDown :: SF a (Yampa.Event Int)
       barsDown = barDownSides >>> Yampa.delay barWidth noEvent
         where barDownSides :: SF a (Yampa.Event Int)
               barDownSides = repeatedly barSep 0

       -- Source of random bar sizes
       justNoise :: SF a Int
       justNoise = noiseR (0, maxBarSize) g

       -- Regular separations
       occasionalEvent :: SF a (Yampa.Event ())
       occasionalEvent = repeatedly barSep ()

-- * App-specific SDL-Yampa bridge

data SDLInput = SDLInput (IORef Controller)

-- | Controller info at any given point.
data Controller = Controller
  { controllerClick :: Bool }

instance Source SDLInput Controller IO where

  initializeSo =
    SDLInput <$> newIORef (Controller { controllerClick = False })

  pollSo (SDLInput cref) = do
    c <- readIORef cref
    c' <- sdlGetController c
    writeIORef cref c'
    return c'

-- We need a non-blocking controller-polling function.
-- TODO: Check http://gameprogrammer.com/fastevents/fastevents1.html
sdlGetController :: Controller -> IO Controller
sdlGetController info =
  foldWhileM info pollEvent (not.isEmptyEvent) ((return .) . handleEvent)

-- | Handles one event only and returns the updated controller.
handleEvent :: Controller -> SDL.Event -> Controller
handleEvent c e =
  case e of
    MouseButtonDown _ _ ButtonLeft           -> c { controllerClick = True }
    MouseButtonUp   _ _ ButtonLeft           -> c { controllerClick = False} 
    KeyDown (Keysym { symKey = SDLK_SPACE }) -> c { controllerClick = True  }
    KeyUp (Keysym { symKey = SDLK_SPACE })   -> c { controllerClick = False }
    _                                        -> c

-- ** SDL renderer as Yampa output consumer (sink)
data MySDLRenderer = MySDLRenderer

instance Sink MySDLRenderer (Int, ([Int],[Int])) IO where

  -- Initialize renderer
  initializeSi = do
    SDL.init [InitVideo]

    _screen <- setVideoMode width height bpp [SWSurface]

    setCaption "Test" ""

    enableUnicode True

    return MySDLRenderer

  -- Rendering each frame
  pushSi MySDLRenderer (playerY, (ceilingHeights,floorHeights)) = do
    screen <- getVideoSurface

    let format = surfaceGetPixelFormat screen

    -- Background
    green <- mapRGB format 0 0xFF 0
    _     <- fillRect screen Nothing green

    -- Paint bars
    red <- mapRGB format 0xFF 0 0
    let paintTopBar (x,h) = fillRect screen (Just (Rect x (height - h) 1 h)) red
        paintBotBar (x,h) = fillRect screen (Just (Rect x 0            1 h)) red
    mapM_ paintBotBar (zip [0 .. width-1] ceilingHeights)
    mapM_ paintTopBar (zip [0 .. width-1] floorHeights)

    -- Paint player
    blue <- mapRGB format 0 0 0xFF
    _    <- fillRect screen (Just $ Rect playerX playerY playerWidth playerHeight) blue

    SDL.flip screen

-- * Game settings
-- Configuration parameters
width :: Num a => a
width = 640

height :: Num a => a
height = 480

bpp :: Num a => a
bpp = 16
       
-- Settings
maxBarSize :: Num a => a
maxBarSize = 200

-- barSep :: Num a => a
barSep = 20 

barWidth :: Num a => a
barWidth = 5

playerHeight :: Num a => a
playerHeight = 32

playerWidth :: Num a => a
playerWidth = 32

playerX :: Num a => a
playerX = 300

stdAcc :: Double
stdAcc = 9.8
