module Colourspace (
    RGBA, HSLA, rgba2hsla, hsla2rgba, distance
)
where

import Codec.Picture (PixelRGBA8(..))

type RGBA a = (a,a,a,a)
type HSLA a = (a,a,a,a)

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

mod' :: (Num a, Ord a) => a -> a -> a
mod' x n
    | x < 0 = mod' (x + n) n
    | x > n = mod' (x - n) n
    | otherwise = x

distanceHSL :: (Eq a, Ord a, Floating a) => RGBA a -> RGBA a -> a
distanceHSL rgba1 rgba2 =
    let
        (h1,s1,l1,_) = rgba2hsla rgba1
        (h2,s2,l2,_) = rgba2hsla rgba2
        (hd,sd,ld) = ((h2-h1)/(2*pi),s2-s1,l2-l1)
    in
        (hd*hd+sd*sd+ld*ld) / 3

distanceRGB :: (Floating a) => RGBA a -> RGBA a -> a
distanceRGB (r1,g1,b1,_) (r2,g2,b2,_) =
    let
        (rd,gd,bd) = (r2-r1,g2-g1,b2-b1)
    in
        (rd*rd + gd*gd + bd*bd) / 3

distance :: (Eq a, Ord a, Floating a) => RGBA a -> RGBA a -> a
distance = distanceHSL


getHue :: (Eq a, Ord a, Floating a) => (a,a,a) -> a
getHue (r,g,b)
    | cmax == r = (pi/3)*(g-b)/(cmax-cmin)
    | cmax == g = (pi/3)*(2 + (b-r)/(cmax-cmin))
    | cmax == b = (pi/3)*(4 + (r-g)/(cmax-cmin))
    where
        cmax = maximum [r,g,b]
        cmin = minimum [r,g,b]

rgba2hsla :: (Eq a, Ord a, Floating a) => RGBA a -> HSLA a
rgba2hsla (r,g,b,a) =
    let
        cmin = minimum [r,g,b]
        cmax = maximum [r,g,b]
        lum = (cmax+cmin) / 2
    in case cmax-cmin of
        0 -> (0, 0, lum, a)
        _ ->
            let
                sat = if' (lum <= 0.5) ((cmax-cmin)/(cmax+cmin)) ((cmax-cmin)/(2-cmax-cmin))
                hue = getHue (r,g,b)
                hue' = if' (hue<0) (hue+2*pi) hue
            in
                (hue',sat,lum,a)

hue2rgb :: (Floating a, Ord a) => (a,a,a) -> (a,a,a)
hue2rgb (h,c,x)
    |      0 <= h && h <   pi/3 = (c,x,0)
    |   pi/3 <= h && h < 2*pi/3 = (x,c,0)
    | 2*pi/3 <= h && h < 3*pi/3 = (0,c,x)
    | 3*pi/3 <= h && h < 4*pi/3 = (0,x,c)
    | 4*pi/3 <= h && h < 5*pi/3 = (x,0,c)
    | 5*pi/3 <= h && h < 6*pi/3 = (c,0,x)

hsla2rgba :: (RealFrac a, Floating a) => HSLA a -> RGBA a
hsla2rgba (h,s,l,a) =
    let
        c = s * ((1-) . abs $ 2*l - 1)
        x = c * ((1-) . abs $ mod' (3*h/pi) 2 - 1)
        m = l - c / 2
        (r,g,b) = hue2rgb (h,c,x)
        f = floor . (*255) . (+m)
    in
        (r,g,b,a)