module SumOfMultiples (sumOfMultiples) where

import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ listOfMultiples factors limit

listOfMultiples :: [Integer] -> Integer -> [Integer]
listOfMultiples factors limit = unique allMultiples
  where allMultiples = concat $ map (\factor -> multiples factor limit) factors

multiples :: Integer -> Integer -> [Integer]
multiples factor limit = takeWhile withinLimit $ stride intFactor [factor..]
  where withinLimit x = x < limit
        intFactor = fromIntegral factor
        stride s = go
          where go (x:xs) = x : go (drop (s - 1) xs)
                go _      = []

unique :: (Ord a) => [a] -> [a]
unique = map head . group . sort
