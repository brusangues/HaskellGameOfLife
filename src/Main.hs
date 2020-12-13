module Main where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import qualified Graphics.Image as I
import System.Environment

import Images   
    ( imgRandom -- imgRandom :: Int -> Int -> IO (Image RPU Y Int)
    , imgBase -- imgBase :: Int -> Int -> Image RPU Y Int
    , imgUpdate
    , img2lists
    , imgRead
    )
import Definitions

---- Parameters
globalCellSize :: Float
globalCellSize = 20
boardSize :: Int
boardSize = 30
fps :: Int
fps = 30

---- Main ------------------------------------------------------------

-- Get initial board configuration
getUserBoard :: IO (I.Image I.VU I.Y Int)
getUserBoard = do
    putStrLn "Choose the initial Life board:"
    putStrLn "(random, block, glider, blinker, triangle)"
    answer <- getLine
    case answer of
        "random"   -> imgRandom boardSize boardSize
        "block"    -> imgRead "images/block.png"
        "glider"   -> imgRead "images/glider.png"
        "blinker"-> imgRead "images/blinker.png"
        "triangle" -> return $ imgBase boardSize boardSize
        _ -> getUserBoard

main :: IO ()
main = do
    args <- getArgs

    -- Chosing 
    img <- if args == ["-p"] || args == ["--params"] then
        getUserBoard
    else
        imgRandom boardSize boardSize

    play                                                           -- play :: 
        windowDisplay                                              -- Display->
        white                                                      -- Color->
        fps                                                        -- Int->
        (World (0, 0) (boundariesMap (boardSize, boardSize)) img)  -- world-> 
        (drawingFunc (globalXOffset, globalYOffset) globalCellSize)-- (world -> Picture)-> 
        inputHandler                                               -- (Event -> world -> world)->
        updateFunc                                                 -- (Float -> world -> world) -> 

---- Calculated parameters
globalXOffset :: Float
globalXOffset = - (globalCellSize * fromIntegral (boardSize-1) )/2
globalYOffset :: Float
globalYOffset = - (globalCellSize * fromIntegral (boardSize-1) )/2

---- Window
windowSize :: Int
windowSize = floor $ globalCellSize * fromIntegral boardSize

windowDisplay :: Display
windowDisplay = InWindow "Life" (windowSize, windowSize) (10, 10)

---- Drawing Function
drawingFunc :: (Float, Float) -> Float -> World -> Picture
drawingFunc (xOffset, yOffset) cellSize world = Pictures [mapGrid, lifePics, playerMarker]
    where
        conversion = location2Coords (xOffset, yOffset) cellSize

        -- Drawing Player
        (px, py) = cellCenter (conversion (playerLocation world))
        playerMarker = translate px py (Color red $ circleSolid (cellSize/2))

        -- Drawing grid
        mapGrid = Pictures $ concatMap makeWallPictures (Map.toList (worldBoundaries world))

        makeWallPictures :: (Location, CellBoundaries) -> [Picture]
        makeWallPictures ((x,y), CellBoundaries up right down left) =
            let coords = conversion (x,y)
                tl@(tlx, tly) = cellTopLeft coords
                tr@(trx, try) = cellTopRight coords
                bl@(blx, bly) = cellBottomLeft coords
                br@(brx, bry) = cellBottomRight coords
            in  [ drawEdge (tr, tl, (tlx, tly - 2), (trx, try - 2)) up
                , drawEdge (br, tr, (trx-2, try), (brx-2, bry)) right
                , drawEdge (bl, br, (brx, bry+2), (blx, bly+2)) down
                , drawEdge (tl, bl, (blx+2, bly), (tlx+2, tly)) left
                ]

        drawEdge :: (Point, Point, Point, Point) -> BoundaryType -> Picture
        drawEdge (p1, p2, _, _) (AdjacentCell _) = Line [p1, p2]
        drawEdge (p1, p2, p3, p4) _ = Color blue (Polygon [p1, p2, p3, p4])

        -- Drawing life cells
        img = lifeImg world
        img' = img2lists img

        lifePics = Pictures $ [locationNcolor2picture (i,j) black | i <- [0..boardSize-1], j <- [0..boardSize-1], lssindex img' i j == 1]

        locationNcolor2picture :: Location -> Color -> Picture
        locationNcolor2picture (x,y) col = 
            let coords = conversion (x,y)
            in Color col (Polygon
                    [ cellTopLeft coords
                    , cellTopRight coords
                    , cellBottomRight coords
                    , cellBottomLeft coords
                    ])

        lssindex :: Num a => [[a]] -> Int -> Int -> a
        lssindex lss i j = lss !! i !! j 

---- Input Handler
inputHandler :: Event -> World -> World
inputHandler event w = case event of
    (EventKey (SpecialKey KeyUp) Down _ _)    -> w { playerLocation = up     , lifeImg = nextImg up    }
    (EventKey (SpecialKey KeyDown) Down _ _)  -> w { playerLocation = down   , lifeImg = nextImg down  }
    (EventKey (SpecialKey KeyRight) Down _ _) -> w { playerLocation = right  , lifeImg = nextImg right }
    (EventKey (SpecialKey KeyLeft) Down _ _)  -> w { playerLocation = left   , lifeImg = nextImg left  }
    (EventKey (SpecialKey KeySpace) Down _ _) -> w { playerLocation = blank_ , lifeImg = nextImg blank_}
    _ -> w
    where
        -- Running life iteration
        nextImg :: Location -> I.Image I.VU I.Y Int
        nextImg loc = img''
            where
                img' = imgUpdate $ lifeImg w
                img'' = I.imap (\(i,j) p -> if (i,j) == loc then I.PixelY 1 else p) img'

        -- Location helper functions
        locAbs (a,b) = if a<0 || b<0 then locInv (a,b) else (a,b)
        locInv (a,b) = (-1-a,-1-b)

        -- Determining next player location
        cellBounds = fromJust $ Map.lookup (locAbs $ playerLocation w) (worldBoundaries w)

        nextLocation :: (CellBoundaries -> BoundaryType) -> Location
        nextLocation boundaryFunc = case boundaryFunc cellBounds of
            (AdjacentCell cell) -> cell
            _ -> playerLocation w
        
        up    = nextLocation upBoundary
        down  = nextLocation downBoundary         
        right = nextLocation rightBoundary
        left  = nextLocation leftBoundary
        blank_= locInv . locAbs $ playerLocation w

---- Update over time

updateFunc :: Float -> World -> World
updateFunc _ = id
