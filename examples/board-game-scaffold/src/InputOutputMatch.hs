-- | This module handles the matching between the user input and the displayed
-- output.
module InputOutputMatch where

-- External imports
import Data.Ix   (inRange)
import Data.List (find)

-- Internal imports
import DeviceOutput (ScreenPosition, gridFieldSize, gridFieldsAbsPositions)
import Objects      (Field, Grid)

-- | Match clicks with displayed fields and return fields if clicked.
clickedFields :: ScreenPosition -> ScreenPosition -> Grid -> (Maybe Field, Maybe Field)
clickedFields click1 click2 grid = (mf1, mf2)
  where
    mf1       = clickedField click1 fldsWithScreenPos
    mf2       = clickedField click2 fldsWithScreenPos

    fldsWithScreenPos = gridFieldsAbsPositions grid
    (fldW,fldH)       = gridFieldSize grid

    -- | Return the field of a grid that was clicked.

    -- If you wonder why the pattern match is only on one argument, search for
    -- `eta conversion` and `pointfree style`.
    clickedField :: ScreenPosition            -- ^ Screen position of the user input (click).
                 -> [(Field, ScreenPosition)] -- ^ The fields of a grid with their screen position.
                 -> Maybe Field               -- ^ The clicked field.
    clickedField click = fmap fst . find (\(_f, screenPos) -> clickInField click screenPos)

    -- | Check whether a click was in a field.
    clickInField :: ScreenPosition -- ^ Position of the click.
                 -> ScreenPosition -- ^ Screen position of the field (left upper corner).
                 -> Bool           -- ^ True, if the position of the click lays within the field.
    clickInField (clickX, clickY) (fldX, fldY) =
         inRange (fldX, fldX + fldW) clickX
      && inRange (fldY, fldY + fldH) clickY
