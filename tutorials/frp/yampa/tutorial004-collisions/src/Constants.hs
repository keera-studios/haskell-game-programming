module Constants where

import Data.Int
import Data.Word
import Graphics.UI.SDL as SDL

width :: Double
width  = 640
height :: Double
height = 480

gameWidth :: Double
gameWidth = width

gameHeight :: Double
gameHeight = height

-- Energy transmission between objects in collisions
velTrans :: Double
velTrans = 1.00

-- Max speed
maxVNorm :: Double
maxVNorm = 500
      
-- Delays
ballWidth, ballHeight :: Double
ballWidth  = 30
ballHeight = 30

ballMargin :: Double
ballMargin = 3

ballSize :: Int16
ballSize = 30

-- Colors
fontColor :: SDL.Color
fontColor = SDL.Color 228 228 228

ballColor :: Word32
ballColor = 0xCC0011FF

velColor  :: Word32
velColor  = 0xCCBBFFFF
