-- | This module defines the levels of the game.
--
-- Each level has conceptual definitions (e.g., initial grid with specified
-- fields and the goal of a level), and visualization details (e.g., color of
-- the background and field colors).
--
-- Together they form 'levels'.
module Levels where

-- External imports

-- Internal imports
import Constants (Color)
import Objects   (Field (Field), FieldId, FieldKind (Empty, Tile), Grid)

-- * Level specification

-- | Level specification with initial grid, background color
--   and colors of fields.
data LevelSpec = LevelSpec
    { -- conceptual definition
      initialGrid :: Grid             -- ^ Initial grid.
      -- visualization details
    , background  :: Color            -- ^ Background color.
    , fieldColor  :: FieldId -> Color -- ^ Field colors.
    }

-- * Concrete levels

-- | List of levels.
levels :: [LevelSpec]
levels = fmap description [0]

-- | Number of available levels.
numLevels :: Int
numLevels = length levels

-- | Specification of levels.
description :: Int -> LevelSpec
-- Level 0
description 0 = LevelSpec ig bg fCs
  where

    -- Background color of the level
    bg = (131, 172, 200, 255)

    -- Fields of the grid
    f1  = Field  1 Tile
    f2  = Field  2 Empty
    f3  = Field  3 Empty
    f4  = Field  4 Tile
    f5  = Field  5 Empty
    f6  = Field  6 Tile
    f7  = Field  7 Tile
    f8  = Field  8 Empty
    f9  = Field  9 Empty
    f10 = Field 10 Tile
    f11 = Field 11 Tile
    f12 = Field 12 Empty
    f13 = Field 13 Tile
    f14 = Field 14 Empty
    f15 = Field 15 Empty
    f16 = Field 16 Tile

    -- Colors of fields
    -- http://www.color-hex.com/color-palette/60787
    -- related colors: (164, 228. 228), (153, 196, 232)
    fCs :: FieldId ->  Color
    fCs  2 = ( 62, 226, 169, 255)
    fCs  3 = ( 62, 226, 169, 255)
    fCs  5 = ( 62, 226, 169, 255)
    fCs  8 = ( 62, 226, 169, 255)
    fCs  9 = ( 62, 226, 169, 255)
    fCs 12 = ( 62, 226, 169, 255)
    fCs 14 = ( 62, 226, 169, 255)
    fCs 15 = ( 62, 226, 169, 255)

    fCs  _ = (117, 219,  48, 255)

    -- Initial grid
    ig = [
          [f1, f5, f9,  f13], [f2, f6, f10, f14]
         ,[f3, f7, f11, f15], [f4, f8, f12, f16]
         ]
-- no more levels available
description _ = error "No more levels!"
