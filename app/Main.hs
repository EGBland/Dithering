module Main where

import Codec.Picture (DynamicImage, convertRGBA8, pixelAt)
import qualified Codec.Picture as P
import Data.List (transpose)
import Text.Printf (printf)

import Dither (bayer4)

type Pixel = (Int,Int,Int)
type Image = [[Pixel]]

readPixel :: P.PixelRGBA8 -> Pixel
readPixel (P.PixelRGBA8 r g b a) = (fromEnum r, fromEnum g, fromEnum b)

readImage :: DynamicImage -> Image
readImage = readImage' . convertRGBA8

readImage' :: P.Image P.PixelRGBA8 -> Image
readImage' (P.Image w h pxs) = map (map readPixel) [[pixelAt (P.Image w h pxs) x y | x <- [0..w-1]] | y <- [0..h-1]]


lightness :: (Floating a) => Pixel -> a
lightness (r,g,b) =
    let
        (r',g',b') = (fromIntegral r, fromIntegral g, fromIntegral b)
    in
        (sqrt $ (r'*r' + g'*g' + b'*b') / 3) / 255

thresh :: (Floating a, Ord a) => [[a]] -> Pixel -> [[Pixel]]
thresh bmat p =
    let
        light = lightness p
    in
        map (map $ \x -> if x <= light then (255,255,255) else (0,0,0)) bmat

-- old broken dither: concat . (map $ map concat) . t . mt . m2t . (map $ map $ thresh bmat)
dither :: (Floating a, Ord a) => [[a]] -> Image -> Image
dither bmat = concat . map ((map concat) . transpose . (map $ thresh bmat))

writePixel :: Pixel -> String
writePixel (r,g,b) = printf "%d\t%d\t%d\n" r g b

writePPM :: Image -> String
writePPM img =
    let
        h = length img
        w = length $ img!!0
    in
        printf "P3\n%d\t%d\t255\n" w h ++ (concat . concat $ map (map writePixel) img)

testimg = replicate 128 [(x,x,x) | x <- [0..255]]


getImage :: FilePath -> IO (Either String Image)
getImage path = P.readImage path >>= (return . (fmap readImage))

doError :: String -> IO ()
doError err = putStrLn err

doSuccess :: Image -> IO ()
doSuccess img = putStrLn . writePPM $ dither bayer4 $ img

main :: IO ()
main = do
    img <- getImage "/home/liz/Pictures/flomoy.jpg"
    case img of
        Left err  -> doError err
        Right img -> doSuccess img
    --putStrLn . writePPM $ dither bayer4 $ testimg
