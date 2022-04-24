module Dither (
    bayer2, bayer4, bayer8,
    dither
)
where

import Codec.Picture (DynamicImage, Image(..), PixelRGBA8(..), Pixel8, convertRGBA8, generateImage, pixelAt)

bayer2 :: (Fractional a) => [[a]]
bayer2 = map (map (/4)) [[0,2],[3,1]]

bayer4 :: (Fractional a) => [[a]]
bayer4 = map (map (/16)) [[0,8,2,10],[12,4,14,6],[3,11,1,9],[15,7,13,5]]

bayer8 :: (Fractional a) => [[a]]
bayer8 = map (map (/64)) [[0,32,8,40,2,34,10,42],[48,16,56,24,50,18,58,26],[12,44,4,36,14,46,6,38],[60,28,52,20,62,30,54,22],[3,35,11,43,1,33,9,41],[51,19,59,27,49,17,57,25],[15,47,7,39,13,45,5,37],[63,31,55,23,61,29,53,21]]

alpha :: PixelRGBA8 -> Pixel8
alpha (PixelRGBA8 _ _ _ a) = a

lightness :: (Floating a) => PixelRGBA8 -> a
lightness (PixelRGBA8 r g b _) =
    let
        conv = fromIntegral . fromEnum
        (r',g',b') = (conv r, conv g, conv b)
    in
        (sqrt $ (r'*r' + g'*g' + b'*b') / 3) / 255

thresh :: (Floating a, Ord a) => [[a]] -> Image PixelRGBA8 -> Int -> Int -> PixelRGBA8
thresh bmat img x y =
    let
        mh = length bmat
        mw = length $ bmat!!0
        mx = mod x mw
        my = mod y mh
        px = pixelAt img x y
        light = lightness px
    in
        if bmat!!my!!mx <= light then (PixelRGBA8 255 255 255 $ alpha px) else (PixelRGBA8 0 0 0 $ alpha px)

dither' :: (Floating a, Ord a) => [[a]] -> Image PixelRGBA8 -> Image PixelRGBA8
dither' bmat (Image w h pxs) = generateImage (thresh bmat $ Image w h pxs) w h

dither :: DynamicImage -> Image PixelRGBA8
dither = dither' bayer4 . convertRGBA8