module RotationalCipher (rotate) where

import Data.Char (ord, chr, isAsciiUpper, isAscii)

rotate :: Int -> String -> String
rotate i = map (rotateChar i)

rotateChar :: Int -> Char -> Char
rotateChar i c
  | isAscii c = i2c' . (+ i) . c2i' $ c
  | otherwise = c
    where (i2c', c2i') = if isAsciiUpper c
                         then (i2cUpper, c2iUpper)
                         else (i2cLower, c2iLower)

c2iUpper, c2iLower :: Char -> Int
c2iUpper = c2i aOrdUpper
c2iLower = c2i aOrdLower

c2i :: Int -> Char -> Int
c2i aOrd c = (ord c - aOrd) `mod` maxIndex

i2cUpper, i2cLower :: Int -> Char
i2cUpper = i2c aOrdUpper
i2cLower = i2c aOrdLower

i2c :: Int -> Int -> Char
i2c aOrd i = chr $ i `mod` maxIndex + aOrd

aOrdUpper, aOrdLower :: Int
aOrdLower = ord 'a'
aOrdUpper = ord 'A'

maxIndex :: Int
maxIndex = 26