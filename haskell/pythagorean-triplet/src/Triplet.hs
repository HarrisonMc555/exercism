module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum target =
  [ (a, b, c)
  | a <- [1..target]
  , b <- [(a + 1)..(target - a)]
  , let c = target - a - b
  , square a + square b == square c
  ]

square :: Int -> Int
square x = x ^ (2 :: Int)