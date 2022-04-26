module Dither (
    dither, greyscale
)
where

import Codec.Picture (DynamicImage, Image(..), PixelRGBA8(..), Pixel8, convertRGBA8, generateImage, pixelAt)
import Data.List (minimumBy)
import Data.Ord (comparing)

type RGBA a = (a,a,a,a)


clamp :: (Ord a) => a -> a -> a -> a
clamp xmin xmax x
    | x < xmin = xmin
    | x > xmax = xmax
    | otherwise = x


bayer2 :: (Fractional a) => [[a]]
bayer2 = map (map (/4)) [[0,2],[3,1]]

bayer4 :: (Fractional a) => [[a]]
bayer4 = map (map (/16)) [[0,8,2,10],[12,4,14,6],[3,11,1,9],[15,7,13,5]]

bayer8 :: (Fractional a) => [[a]]
bayer8 = map (map (/64)) [[0,32,8,40,2,34,10,42],[48,16,56,24,50,18,58,26],[12,44,4,36,14,46,6,38],[60,28,52,20,62,30,54,22],[3,35,11,43,1,33,9,41],[51,19,59,27,49,17,57,25],[15,47,7,39,13,45,5,37],[63,31,55,23,61,29,53,21]]


normalise :: (Fractional a) => PixelRGBA8 -> RGBA a
normalise (PixelRGBA8 r g b a) = (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255, fromIntegral a / 255)

denormalise :: (RealFrac a) => RGBA a -> PixelRGBA8
denormalise (r,g,b,a) = let f = (clamp 0 255) . floor . (*255) in PixelRGBA8 (f r) (f g) (f b) (f a)

distance :: (Floating a) => RGBA a -> RGBA a -> a
distance (r1,g1,b1,_) (r2,g2,b2,_) =
    let
        (rd,gd,bd) = (r2-r1,g2-g1,b2-b1)
    in
        sqrt $ (rd*rd + gd*gd + bd*bd) / 3

lightness :: (Floating a) => RGBA a -> a
lightness px = sqrt $ distance (0,0,0,1) px

nearestColour :: (Ord a, Floating a) => [RGBA a] -> RGBA a -> RGBA a
nearestColour palette (r,g,b,a) =
    let
        (nr,ng,nb,_) = minimumBy (comparing $ distance (r,g,b,a)) palette
    in
        (nr,ng,nb,a)

thresh :: (RealFrac a, Floating a, Ord a) => [RGBA a] -> [[a]] -> Image PixelRGBA8 -> Int -> Int -> PixelRGBA8
thresh palette bmat img x y =
    let
        mh = length bmat
        mw = length $ bmat!!0
        mx = mod x mw
        my = mod y mh
        off = bmat!!my!!mx-0.5

        (r,g,b,a) = normalise $ pixelAt img x y
        (r',g',b') = (r+off/3,g+off/3,b+off/3)
    in
        denormalise . (nearestColour palette) $ (r',g',b',a)

dither' :: (RealFrac a, Floating a, Ord a) => [RGBA a] -> [[a]] -> Image PixelRGBA8 -> Image PixelRGBA8
dither' palette bmat (Image w h pxs) = generateImage (thresh palette bmat $ Image w h pxs) w h

dither :: DynamicImage -> Image PixelRGBA8
dither = dither' [(0,0,0,1),(1,1,1,1)] bayer4 . convertRGBA8

greyscale :: DynamicImage -> Image PixelRGBA8
greyscale = dither' [let y = fromIntegral x / 255 in (y,y,y,1) | x <- [0..255]] [[0.5]] . convertRGBA8