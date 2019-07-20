module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean triplet = square x + square y == square z
  where (x, y, z) = orderedTriplet triplet

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet a b c = (a, b, c)

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets minFactor maxFactor = 
  [ mkTriplet a b c
  | a <- [minFactor.. maxFactor]
  , b <- [a.. maxFactor]
  , let c = isqrt $ square a + square b
          , c <= maxFactor
          , isPythagorean $ mkTriplet a b c
  ]

orderedTriplet :: (Int, Int, Int) -> (Int, Int, Int)
orderedTriplet (a, b, c)
  | y <= c    = (x, y, c)
  | x <= c    = (x, c, y)
  | otherwise = (c, x, y)
  where (x, y) = orderedPair (a, b)

orderedPair :: (Int, Int) -> (Int, Int)
orderedPair (a, b)
  | a <= b    = (a, b)
  | otherwise = (b, a)

isqrt :: Int -> Int
isqrt x = floor . sqrt $ (fromIntegral x :: Float)

square :: Int -> Int
square x = x ^ (2 :: Int)