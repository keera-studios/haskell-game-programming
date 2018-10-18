-- | Definitions of constants.
module Constants where

-- External imports
import Data.Word (Word8)

-- Internal imports

-- | Default caption of the game.
caption :: String
caption = "Keera-Board-Game"

-- | Default width and height of the (squared) screen.
screenWH :: Num a => a
screenWH = 660

-- | Default margin of the screen (2 % of the screen size).
screenMarginWH :: Num a => a
screenMarginWH = rDoubleToNum screenWH'
  where
    screenWH' :: Double
    screenWH' = (screenWH / 100) * 2

-- | The default background color.
defaultBG :: Color
defaultBG = (131, 172, 200, 255)

-- * Auxiliary types

-- | A RGBA color definition.
--
-- Red, green and blue range from 0 - 255 to define the intensity of the color
-- with 0 no intensity and 255 full itensity.  The last value is the alpha
-- channel to define the opacity of the color with 0 for fully transparent and
-- 255 for fully opaque.

-- Convinient synonym for avoiding writing Word8 four times.
type Color = (Word8, Word8, Word8, Word8)

-- * Auxiliary functions

-- | Round doubles and convert the result to num.
rDoubleToNum :: Num a => Double -> a
rDoubleToNum n = fromIntegral (round n :: Integer)

-- * Debugging

-- | Initial level. Change this in the code to start
-- from a different existing level.
initialLevel :: Int
initialLevel = 0
