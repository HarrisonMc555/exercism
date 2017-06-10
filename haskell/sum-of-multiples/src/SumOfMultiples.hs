module SumOfMultiples (sumOfMultiples) where

import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ listOfMultiples factors limit

listOfMultiples :: [Integer] -> Integer -> [Integer]
listOfMultiples factors limit = unique allMultiples
  where allMultiples = concatMap (`multiples` limit) factors

multiples :: Integer -> Integer -> [Integer]
multiples factor limit = takeWhile (< limit) $ stride intFactor [factor..]
  where intFactor = fromIntegral factor
        stride _ []     = []
        stride s (x:xs) = x : stride s rest
          where rest = drop (s-1) xs

unique :: (Ord a) => [a] -> [a]
unique = map head . group . sort
