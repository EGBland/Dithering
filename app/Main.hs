module Main where

import Codec.Picture (DynamicImage, readImage, writePng)
import System.Environment (getArgs)

import Dither (dither, greyscale)

doError :: String -> IO ()
doError err = putStrLn err

doSuccess :: DynamicImage -> FilePath -> IO ()
doSuccess img outp = writePng outp $ dither img

tryDither :: FilePath -> FilePath -> IO ()
tryDither path outp = do
    img <- readImage path
    case img of
        Left err  -> doError err
        Right img -> doSuccess img outp

parseArgs :: [String] -> IO ()
parseArgs [inp,outp] = tryDither inp outp
parseArgs _ = usage

main :: IO ()
main = getArgs >>= parseArgs

usage :: IO ()
usage = putStrLn "Usage: dither <input image> <output destination>"