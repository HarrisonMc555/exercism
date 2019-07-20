module PerfectNumbers (classify, Classification(..)) where

import Data.Numbers.Primes (primeFactors)
import Data.List (tails, group)

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify i = classify' <$> aliquot i
  where classify' a
          | a > i     = Abundant
          | a < i     = Deficient
          | otherwise = Perfect

aliquot :: Int -> Maybe Int
aliquot = go
  where go i = sum <$> divisors i

divisors :: Int -> Maybe [Int]
divisors x
  | x <  1 = Nothing
  | x == 1 = Just []
  | otherwise = let ps      = primeFactors x
                    pgroups = group ps
                in Just . tail . map product . partitions $ pgroups

partitions :: [[a]] -> [[a]]
partitions [] = [[]]
partitions (xs:xss) = let rest = partitions xss
                      in concatMap (\xs' -> map (xs' ++) rest) $ tails xs