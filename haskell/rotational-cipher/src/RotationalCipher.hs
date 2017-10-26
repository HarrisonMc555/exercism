module RotationalCipher (rotate) where

import Data.Char (ord, chr, isLetter)

rotate :: Int -> String -> String
rotate i = map (rotateChar i)

rotateChar :: Int -> Char -> Char
rotateChar i c
  | not (isLetter c) = error "non-letter in rotateChar"
  | otherwise        = i2c . (+ i) . c2i $ c

c2i :: Char -> Int
c2i c = (ord c - aOrd) `mod` maxOrd

i2c :: Int -> Char
i2c i = chr ((i `mod` maxOrd) + aOrd)

aOrd, maxOrd :: Int
aOrd = ord 'a'
maxOrd = 26
