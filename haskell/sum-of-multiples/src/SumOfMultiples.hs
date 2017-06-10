module SumOfMultiples (sumOfMultiples) where

import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ listOfMultiples factors limit

listOfMultiples :: [Integer] -> Integer -> [Integer]
listOfMultiples factors limit = unique allMultiples
  where allMultiples = concatMap (`multiples` limit) factors

multiples :: Integer -> Integer -> [Integer]
multiples factor limit = takeWhile (< limit) [factor,factor*2..]

unique :: (Ord a) => [a] -> [a]
unique = map head . group . sort
