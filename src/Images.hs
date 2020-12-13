module Images
( imgRandom
, imgBase
, imgConvolved
, imgBoolean
, imgUpdate
, img2lists
, imgRead
) where

import Graphics.Image as I
--import System.IO
import System.Random
import Control.Monad (replicateM)

---- Random Image

randomPixs :: Int -> IO [Pixel Y Int]
randomPixs 0 = return []
randomPixs n = do
    r  <- randomRIO (0,1) :: IO Int
    let r' = PixelY $ fromIntegral r
    rs <- randomPixs(n-1)
    return (r':rs)

imgRandom :: Int -> Int -> IO (Image VU Y Int)
imgRandom x y = do
    a <- replicateM y $ randomPixs x
    return $ fromLists a

---- Base Image

imgBase :: Int -> Int -> Image VU Y Int
imgBase x y = makeImageR VU (x, y) (\(i, j) -> PixelY $ if i>=j then 1 else 0)
-- makeImageR :: arr -> (Int, Int) -> ((Int, Int) -> Pixel cs e) -> Image arr cs e

---- Covolution

-- fromLists :: Array arr cs e => [[Pixel cs e]] -> Image arr cs e
kernel :: Image VU X Int
kernel = fromLists [[1,1,1],[1,10,1],[1,1,1]]

imgConvolved :: Image VU Y Int -> Image VU Y Int
imgConvolved img = convolve Wrap kernel img -- Image VU Y Int

---- Boolean

-- map :: (Array arr cs e, Array arr cs' e') => 
-- (Pixel cs' e' -> Pixel cs e)	-> Image arr cs' e'	-> Image arr cs e
imgBoolean :: Image VU Y Int -> Image VU Y Int
imgBoolean img = I.map (\(PixelY y) -> PixelY $ if y == 3 || y == 12 || y == 13 then 1 else 0 ) img

---- Update function

imgUpdate :: Image VU Y Int -> Image VU Y Int
imgUpdate img = imgBoolean $ imgConvolved img

---- Image to Lists

pix2num :: Num a => Pixel Y a -> a
pix2num (PixelY x) = x

--toLists :: MArray arr cs e => Image arr cs e -> [[Pixel cs e]]
img2lists :: Image VU Y Int -> [[ Int ]]
img2lists img = Prelude.map (Prelude.map pix2num) lists
   where
       lists = I.toLists img

---- Reading Images

imgRead :: String -> IO (Image VU Y Int)
imgRead path = do
    img <- readImageY VU path
    let img' = I.map (\(PixelY e) -> PixelY $ abs (1 - ceiling e)  ) img
    return img'

---- IO
{-
lifeRun :: Int -> Image VU Y Int -> IO ()
lifeRun 0 _ = return ()
lifeRun n img = do
    let img' = imgUpdate img
    I.displayImage (img' :: Image VU Y Int)
    threadDelay 5000000
    lifeRun (n-1) img'
    return ()


main :: IO ()
main = do

    img <- imgRandom 40 20
    let img' = imgUpdate img

    -- displayImage :: Image arr cs e -> IO ()
    I.displayImage (img :: Image VU Y Int)
    --I.displayImage (img' :: Image VU Y Int)
    lifeRun 10 img

    trash <- getLine
    if trash == "" then do
        main
    else do
        return ()
    return ()

-}