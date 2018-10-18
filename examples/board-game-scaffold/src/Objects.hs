-- | Defines the game objects and their functionallity.
--
-- It is the grid with its fields, a function to change
-- the grid and functions to find fields in the grid.
module Objects where

-- External imports
import Data.List (elemIndex, findIndex)
import Data.Maybe (fromJust)

-- Internal imports

-- * Board grids

-- | A grid is defined as a list of columns.
type Grid = [Column]

-- | A column is defined as a list of fields.
type Column = [Field]

-- | A row is defined as a list of fields.
type Row = [Field]

-- | A field has logical properties (ID, kind) and object properties.
data Field = Field { fieldId   :: FieldId
                   , fieldKind :: FieldKind
                   }
  deriving (Show, Eq)

-- | The field id.
type FieldId = Int

-- | The kind of field.
--   Either a field "contains" a tile that can be moved
--   or the field is empty.
data FieldKind = Tile | Empty
  deriving (Show, Eq)

-- * Change grid

-- | Swap two fields in a grid.
--
-- PRE: Both fields need to be in the grid.
swapFields :: Field -> Field -> Grid -> Grid
swapFields f1 f2 = map (map choose)
  where
   choose :: Field -> Field
   choose cel | fieldId cel == fieldId f1 = f2
              | fieldId cel == fieldId f2 = f1
              | otherwise                 = cel

-- * Fields and their positions

-- | The position (column, row) of a field in a grid,
-- where (0,0) means the first column and first row in the grid.
type FieldPosition = (Int, Int)

-- | Find a field in a grid by its field id.
--
-- PRE: The field needs to be in the grid.

-- If you wonder why the pattern match is only on one argument, search for `eta
-- conversion` and `pointfree style`.
getFieldById :: FieldId -> Grid -> Field
getFieldById fldId = head . concatMap (filter (\fld -> fieldId fld == fldId))

-- | Get field by position.
--
-- PRE: FieldPosition must be within the grid (size).
getFieldByPosition :: FieldPosition -> Grid -> FieldId
getFieldByPosition (column, row) grid = fieldId $ (grid !! column) !! row

-- | Find the position of a field in a grid.
--
-- PRE: Field needs to be in the grid.
--
-- Gets a Grid g and a Field f returns the Position (column, row) of f in g
-- using (0,0) as first column, first row.
findPosition :: Grid -> Field -> FieldPosition
findPosition grid fld = (columnPos, rowPos)
  where
    columnPos = findColumnPosition fld  grid
    rowPos    = findRowPosition    fld (grid !! columnPos)

-- * Auxiliary functions

-- | Find the column of a field in a grid.
--
-- PRE: Field needs to be in grid. Consequently, grid must not be empty.
--
-- Given a Grid g and a Field f and a starting count value
-- return the column (index) of f in g.

-- If you wonder why the pattern match is only on one argument, search for `eta
-- conversion` and `pointfree style`.
findColumnPosition :: Field -> Grid -> Int
findColumnPosition f = fromJust . findIndex (elem f)

-- | Find the row of a field in a column.
--
-- PRE: Field needs to be in column. Consequently, column must not be empty.
--
-- Given a Column c of a Grid, a Field f and a starting count value
-- return the row (index) of f in c.

-- If you wonder why the pattern match is only on one argument, search for `eta
-- conversion` and `pointfree style`.
findRowPosition :: Field -> [Field] -> Int
findRowPosition f = fromJust . elemIndex f
