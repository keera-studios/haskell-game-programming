-- IDEAS: when a sheet is loaded, add the FPS for the animation
-- and pass the delta time to the function that moves animations
-- forward
--
-- Make managers monads, or better, monad transformers.
--
-- One will be IO-based, the other won't

import           Control.Monad
import           Data.IORef
import           Data.List
import qualified Data.Map              as M
import           Data.Word
import           Graphics.UI.SDL       as SDL
import           Graphics.UI.SDL.Image as SDL

main = do
  SDL.init [InitVideo]
  screen <- SDL.setVideoMode 480 320 32 [HWSurface, DoubleBuf]

  spriteAnim <- loadTimedSpriteAnim "assets/multimedia/animation/pftcc.png" 80 14 coordsF

  let assetMgr                    = mkAssetManager
      visualMgr                   = mkVisualManager
      (n', visualMgr', assetMgr') = registerAsset assetMgr visualMgr (TimeBasedAnim spriteAnim)

  clock <- newSDLClock

  lastTime <- readIORef clock

  go assetMgr' visualMgr' n' clock lastTime

go assetMgr visualMgr spriteAnim clock last = do

    -- Constant FPS
    -- SDL.delay 10
    delayNextFrame 16 clock

    -- Animations (pure)
    ticks <- getTicks 
    let dt = fromIntegral (ticks - last)
    let visualMgr' = advanceAllAnim assetMgr visualMgr dt
    print dt

    -- Rendering
    screen <- getVideoSurface

    let format = SDL.surfaceGetPixelFormat screen
    white <- SDL.mapRGB format 0xFF 0xFF 0xFF
    SDL.fillRect screen Nothing white

    showSpriteAnim assetMgr visualMgr' screen spriteAnim

    SDL.flip screen

    -- Loop
    go assetMgr visualMgr' spriteAnim clock ticks

type SpriteSheetFrameArea = Int -> Rect

coordsF :: SpriteSheetFrameArea
coordsF = spriteSheetCoords 5 73 68

-- * Coordinates of sprite in left-to-right top-bottom equal-size sprite sheet
spriteSheetCoords :: Int -> Int -> Int -> Int -> Rect
spriteSheetCoords cols w h n = Rect x y w h
 where x  = c * w
       y  = r * h
       c  = n `mod` cols
       r  = n `div` cols

-- * Visual manager and asset manager
-- Visual manager is pure, asset manager is impure
type VisualManagerInfo = M.Map VisualId VisualRepr

type VisualId   = Int
data VisualRepr = SpriteAnimationState (AssetId, Int)
                | TimeSpriteAnimationState  (AssetId, Int, Int)

mkVisualManager :: VisualManagerInfo
mkVisualManager = M.empty

visualManagerRegister :: VisualManagerInfo -> VisualRepr -> (VisualId, VisualManagerInfo)
visualManagerRegister m r = (newReprId, m')
 where m'        = M.insert newReprId r m
       newReprId = nextFreeKey m 0

-- * Asset manager
mkAssetManager :: AssetManagerInfo
mkAssetManager = M.empty

type AssetManagerInfo = M.Map AssetId AssetRepr

assetManagerRegister :: AssetManagerInfo -> AssetRepr -> (AssetId, AssetManagerInfo)
assetManagerRegister m r = (newAssetId, m')
 where m'         = M.insert newAssetId r m
       newAssetId = nextFreeKey m 0

nextFreeKey :: (Ord k, Enum k) => M.Map k v -> k -> k
nextFreeKey m k = firstHole k sortedKeys 
 where sortedKeys = sort (M.keys m)

       -- | Find the first hole in a list
       firstHole :: (Ord r, Enum r) => r -> [r] -> r
       firstHole n [] = n
       firstHole n (m:ms)
          | n < m     = n
          | otherwise = firstHole (succ n) ms

type AssetId = Int
data AssetRepr  = SpriteAnim SpriteAnim
                | TimeBasedAnim TimeBasedAnim

type SpriteAnim = ( SDL.Surface -- Sheet
                  , Int         -- Num frames
                  , [Rect]      -- Coordinates
                  )

type TimeBasedAnim = (SpriteAnim, Int) -- MS per frame

-- IDEAS: add a displacement vector
loadSpriteAnim :: FilePath -> Int -> (Int -> Rect) -> IO SpriteAnim
loadSpriteAnim fp numFrames frameCoords = do
  sheet <- load fp
  let frames = map frameCoords [0..numFrames-1]
  return (sheet, numFrames, frames)

-- IDEAS: add a displacement vector
loadTimedSpriteAnim :: FilePath -> Int -> Int -> (Int -> Rect) -> IO TimeBasedAnim
loadTimedSpriteAnim fp ms numFrames frameCoords = do
  sheet <- load fp
  let frames = map frameCoords [0..numFrames-1]
  return ((sheet, numFrames, frames), ms)

advanceAllAnim :: AssetManagerInfo -> VisualManagerInfo -> Int -> VisualManagerInfo
advanceAllAnim assetMgr visualMgr ms =
  fmap (advanceSpriteAnim assetMgr ms) visualMgr

advanceSpriteAnim :: AssetManagerInfo -> Int -> VisualRepr -> VisualRepr
advanceSpriteAnim assetManagerInfo ms info@(TimeSpriteAnimationState (asset, n, lms)) =
  case M.lookup asset assetManagerInfo of
    (Just (TimeBasedAnim ((_, l, _),mms))) -> let numCycles = (lms + ms) `div` mms
                                                  restMS    = (lms + ms) `mod` mms
                                              in TimeSpriteAnimationState (asset,  (n+numCycles) `mod` l, restMS)
    _                                     -> info
  

advanceSpriteAnim assetManagerInfo ms info@(SpriteAnimationState (asset, n)) =
  case M.lookup asset assetManagerInfo of
    (Just (SpriteAnim (_, l, _))) -> SpriteAnimationState (asset, (n+1) `mod` l)
    _                             -> info

-- ** Rendering
showSpriteAnim :: AssetManagerInfo -> VisualManagerInfo -> Surface -> VisualId -> IO ()
showSpriteAnim assetMgr visualMgr screen anim = do
   case M.lookup anim visualMgr of
     Nothing -> return ()
     
     Just (TimeSpriteAnimationState (a, f,oms)) -> case M.lookup a assetMgr of
                      Just (TimeBasedAnim ((s, _, r),_)) -> do
                        let rect = r!!f
                        SDL.blitSurface s (Just rect) screen Nothing
                        return ()
                      _ -> return ()
     Just (SpriteAnimationState (a, f)) -> case M.lookup a assetMgr of
                      Just (SpriteAnim (s, _, r)) -> do
                        let rect = r!!f
                        SDL.blitSurface s (Just rect) screen Nothing
                        return ()
                      _ -> return ()

registerAsset :: AssetManagerInfo -> VisualManagerInfo -> AssetRepr -> (VisualId, VisualManagerInfo, AssetManagerInfo)
registerAsset assetMgr visualMgr asset@(TimeBasedAnim _) = (n', visualMgr', assetMgr')
 where (n', visualMgr') = visualManagerRegister visualMgr (TimeSpriteAnimationState (n, 0, 0))
       (n,  assetMgr')  = assetManagerRegister  assetMgr asset
registerAsset assetMgr visualMgr asset@(SpriteAnim _) = (n', visualMgr', assetMgr')
 where (n', visualMgr') = visualManagerRegister visualMgr (SpriteAnimationState (n, 0))
       (n,  assetMgr')  = assetManagerRegister  assetMgr asset

-- * Abstract SDL Clock
type SDLClock = IORef Word32

newSDLClock = do
  n <- SDL.getTicks
  newIORef n

updateSDLClock clock = do
  n <- SDL.getTicks
  writeIORef clock n

delayNextFrame :: Word32 -> SDLClock -> IO ()
delayNextFrame ms clock = do
  n <- SDL.getTicks
  o <- readIORef clock
  let diff = n - o
  when (ms > diff) $ SDL.delay (ms - diff)
  writeIORef clock n
