-- A demo game put together in less than 2 hours to show how easy
-- game programming is in Haskell.
--
-- If you want to use the wiimote, compile the game with the flag
-- -rtsopts
-- and run it with
-- +RTS -V0
-- import Control.Concurrent
import Control.Monad
import Graphics.UI.SDL       as SDL
import Graphics.UI.SDL.Image as SDL
import Graphics.UI.SDL.TTF   as TTF
import System.CWiid
import System.Random

-- * Game State
data GameState = GameState
 { raindrops    :: [(Float, Float)]
 , paddle       :: (Float, Float)
 , lives        :: Int
 , points       :: Int
 , lastRaindrop :: Float
 , lastFrame    :: Float -- Time of last frame
 , randomGen    :: StdGen
 }
 deriving Show

level :: GameState -> Int
level gameState = points gameState `div` dropsPerLevel

updatePaddlePos :: GameState -> Float -> GameState
updatePaddlePos gameState newX =
  gameState { paddle = (min newX (width - paddleW), height - paddleMargin) }

initialGameState :: StdGen -> GameState
initialGameState gen =
  GameState [(width / 2, dropMargin)] (0, height - paddleMargin) maxLives 0 0 0 gen

-- * Runtime environment (resources, devices)
data Env = Env
 { bgImg     :: Surface
 , dropImg   :: Surface
 , paddleImg :: Surface
 , font      :: TTF.Font
 , cwiid     :: Maybe CWiidWiimote
 }

main :: IO ()
main = do
  -- Initialise SDL
  SDL.init [InitEverything]
  ttfOk <- TTF.init

  when ttfOk $ do
    font   <- TTF.tryOpenFont "data/font.ttf" 32
    bg     <- load "data/background.png"

    -- Load with a mask
    drop   <- load "data/cherry.png"
    t <- mapRGB (surfaceGetPixelFormat drop) 0 255 0 
    setColorKey drop [SrcColorKey, RLEAccel] t

    -- Load with a mask
    paddle <- load "data/player.png"
    t <- mapRGB (surfaceGetPixelFormat paddle) 0 255 0 
    setColorKey paddle [SrcColorKey, RLEAccel] t

    -- Create window, no mouse
    SDL.setVideoMode (round width) (round height) 32 [SWSurface]
    SDL.setCaption "Raindrops" ""
    SDL.showCursor False

    -- Create Random number generator
    gen <- getStdGen

    -- Initialise input devices
    wiimote <- initializeWiimote

    case font of
      Nothing  -> return ()
      Just ttf -> let env = Env bg drop paddle ttf wiimote
                      gs  = initialGameState gen
                  in run env gs

-- Game loop
run :: Env -> GameState -> IO ()
run env gameState = do
  -- IO: Sense (input)
  gameStateP <- calculatePaddlePos env gameState

  -- IO: Sense (time)
  newTime <- fmap fromIntegral SDL.getTicks
  let dt = newTime - lastFrame gameStateP
  let gameStateT = gameStateP { lastFrame = newTime }

  -- Physics (movement)
  let gameStateP = moveForward dt gameStateT

  -- Physics (collisions)
  let gameStateC = raindropsBottom (raindropsPaddle gameStateP)

  -- Logic (new raindrops)
  let dtLastRaindrop = newTime - lastRaindrop gameStateC
      gameStateN =
        if dtLastRaindrop > dropDelay
          then let (newX, gen') = randomR (0, round (width - dropW) :: Int)
                                          (randomGen gameStateC)
                   oldDrops = raindrops gameStateC
               in gameStateC { raindrops    = (fromIntegral newX, dropMargin) : oldDrops
                             , lastRaindrop = newTime
                             , randomGen    = gen'
                             }
          else gameStateC

  -- Logic (game over)
  let gameStateO = if lives gameStateN < 0
                     then initialGameState (randomGen gameStateN)
                     else gameStateN

  -- IO: Paint
  render env gameStateO

  -- Loop
  run env gameStateO

-- * Physics
moveForward :: Float -> GameState -> GameState
moveForward dt gs = gs { raindrops = movedRaindrops }
  where movedRaindrops     = map moveRaindrop (raindrops gs)
        moveRaindrop (x,y) = (x, y + 0.1 * dt * fromIntegral (level gs + 1))

-- * Collisions

-- ** Collisions with paddle
raindropsPaddle :: GameState -> GameState
raindropsPaddle gs = gs { raindrops = remainingRaindrops
                        , points    = points gs + pts
                        } 
 where remainingRaindrops       = filter (not.collidesWithPaddle) (raindrops gs)
       pts                      = length (raindrops gs) - length remainingRaindrops
       collidesWithPaddle (x,y) = (within x paddleXMin paddleXMax
                                  || within paddleXMin x (x + dropW))
                                  && (within y paddleYMin paddleYMax
                                     || within paddleYMin y (y + dropH))
         where paddleXMin = fst (paddle gs)
               paddleXMax = fst (paddle gs) + paddleW
               paddleYMin = snd (paddle gs)
               paddleYMax = snd (paddle gs) + paddleH
               within x xMin xMax = x >= xMin && x <= xMax

-- ** Collisions with bottom
raindropsBottom :: GameState -> GameState
raindropsBottom gs = gs { raindrops = remainingRaindrops
                        , lives     = decreasedLives
                        }
 where remainingRaindrops = filter (\(_,y) -> y < height) (raindrops gs)
       decreasedLives     = lives gs - diffDrops
       diffDrops          = length (raindrops gs) - length remainingRaindrops

-- * Input sensing
calculatePaddlePos :: Env -> GameState -> IO GameState
calculatePaddlePos env gs = case cwiid env of
 Nothing -> calculatePaddlePosSDL env gs
 Just wm -> do (x,y) <- senseWiimote wm
               return (updatePaddlePos gs x)

-- ** SDL Sensing
calculatePaddlePosSDL :: Env -> GameState -> IO GameState
calculatePaddlePosSDL env gs = do
  e <- pollEvent
  case e of
    NoEvent             -> return gs
    MouseMotion x y _ _ -> calculatePaddlePosSDL env (updatePaddlePos gs (fromIntegral x))
    _                   -> calculatePaddlePosSDL env gs

-- * Output drawing
render :: Env -> GameState -> IO ()
render env gameState = do

  screen <- getVideoSurface

  -- Clear screen
  SDL.blitSurface (bgImg env) Nothing
                  screen (Just (SDL.Rect 0 0 (round width) (round height)))

  -- Paint each raindrop
  let paintADropAt (x,y) = do
        SDL.blitSurface (dropImg env) Nothing
                        screen (Just (SDL.Rect (round x) (round y) (round dropW) (round dropH)))

  mapM_ paintADropAt (raindrops gameState)

  -- Paint the paddle
  let (x,y) = paddle gameState
  SDL.blitSurface (paddleImg env) Nothing
                  screen (Just (SDL.Rect (round x) (round y) (round paddleW) (round paddleH)))

  -- Paint points, lives
  let ttf = font env
  message <- TTF.renderTextSolid
               ttf
               ("Level " ++ show (level gameState)
                ++ " / Lives " ++ show (lives gameState))
               (SDL.Color 128 128 128)
  let w1 = SDL.surfaceGetWidth  message
      h1 = SDL.surfaceGetHeight message
  SDL.blitSurface message Nothing
                  screen (Just (SDL.Rect 10 10 w1 h1))

  -- Present
  SDL.flip screen

-- * Game constants
paddleW, paddleH :: Float
paddleW = 126
paddleH = 31

paddleMargin :: Float
paddleMargin = 60

width, height :: Float
width         = 800
height        = 480

dropW, dropH :: Float
dropW        = 70
dropH        = 70

dropMargin :: Float
dropMargin = 10

dropsPerLevel :: Int
dropsPerLevel = 20

dropDelay :: Float
dropDelay = 500

maxLives :: Int
maxLives = 10

-- * Wiimote sensing

-- | Initializes the wiimote, optionally returning the sensing function. It
-- returns Nothing if the Wiimote cannot be detected. Users should have a BT
-- device and press 1+2 to connect to it. A message is shown on stdout.
initializeWiimote :: IO (Maybe CWiidWiimote)
initializeWiimote = do
  putStrLn "Initializing WiiMote. Please press 1+2 to connect."
  wm <- cwiidOpen
  case wm of
    Nothing  -> return ()
    Just wm' -> void $ cwiidSetRptMode wm' 15 -- Enable button reception, acc and IR
  return wm

senseWiimote :: CWiidWiimote -> IO (Float, Float)
senseWiimote wmdev = do
  irs   <- cwiidGetIR wmdev

  -- Obtain positions of leds 1 and 2 (with a normal wii bar, those
  -- will be the ones we use).
  let led1   = irs!!0
      led2   = irs!!1

  -- Calculate mid point between sensor bar leds
  let posX = ((cwiidIRSrcPosX led1) + (cwiidIRSrcPosX led2)) `div` 2
      posY = ((cwiidIRSrcPosY led1) + (cwiidIRSrcPosY led2)) `div` 2

  -- Calculate proportional coordinates
  let propX = fromIntegral (1024 - posX) / 1024.0
      propY = fromIntegral (max 0 (posY - 384)) / 384.0

  -- Calculate game area coordinates
  let finX  = width  * propX
      finY  = height * propY

  return (finX, finY)
