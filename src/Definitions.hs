module Definitions where

import Graphics.Gloss ( Point )
import qualified Graphics.Image as I
import qualified Data.Map as Map
import Data.Ix (range)

---- Type and Data Definitions
type Location = (Int, Int)

data CellCoordinates = CellCoordinates
    { cellCenter :: Point
    , cellTopLeft :: Point
    , cellTopRight :: Point
    , cellBottomLeft :: Point
    , cellBottomRight :: Point
    }

data BoundaryType = WorldBoundary | Wall | AdjacentCell Location
    deriving (Show, Eq)

data CellBoundaries = CellBoundaries
    { upBoundary :: BoundaryType
    , rightBoundary :: BoundaryType
    , downBoundary :: BoundaryType
    , leftBoundary :: BoundaryType
    }
    deriving (Show, Eq)

type DirectionLens = CellBoundaries -> BoundaryType

data World = World
    { playerLocation :: Location
    , worldBoundaries :: Map.Map Location CellBoundaries
    , lifeImg :: I.Image I.VU I.Y Int
    }

---- Boundaries for the Window

boundariesMap :: (Int, Int) -> Map.Map Location CellBoundaries
boundariesMap (numColumns, numRows) = Map.fromList (buildBounds <$> range ((0,0), (numColumns, numRows)))
    where
        buildBounds :: Location -> (Location, CellBoundaries)
        buildBounds loc = (loc, simpleBoundaries (numColumns, numRows) loc)

simpleBoundaries :: (Int, Int) -> Location -> CellBoundaries
simpleBoundaries (numColumns, numRows) (x, y) = CellBoundaries
    (if y + 1 < numRows then AdjacentCell (x, y+1) else WorldBoundary)
    (if x + 1 < numColumns then AdjacentCell (x+1, y) else WorldBoundary)
    (if y > 0 then AdjacentCell (x, y-1) else WorldBoundary)
    (if x > 0 then AdjacentCell (x-1, y) else WorldBoundary)

---- Drawing Function Helper 

location2Coords :: (Float, Float) -> Float -> Location -> CellCoordinates
location2Coords (xOffset, yOffset) cellSize (x, y) = CellCoordinates
    (centerX, centerY) -- Center
    (centerX - halfCell, centerY + halfCell) -- Top Left
    (centerX + halfCell, centerY + halfCell) -- Top Right
    (centerX - halfCell, centerY - halfCell) -- Bottom Left
    (centerX + halfCell, centerY - halfCell) -- Bottom Right
    where
        (centerX, centerY) = (xOffset + (fromIntegral x) * cellSize, yOffset + (fromIntegral y) * cellSize)
        halfCell = cellSize / 2.0