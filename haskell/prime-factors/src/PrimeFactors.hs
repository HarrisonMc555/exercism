{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrimeFactors (primeFactors) where

import Data.List (find)
import Data.Maybe (fromJust)
-- import Data.Numbers.Primes (primes)

primeFactors :: Integer -> [Integer]
primeFactors n = getResult . find done . scanl addFactors (n, []) $ primes
  where done = (1 ==) . fst
        getResult = reverse . snd . fromJust

addFactors :: (Integer, [Integer]) -> Integer -> (Integer, [Integer])
addFactors (n, fs) p
  | n == 1    = (n, fs)
  | composite = addFactors (q, p:fs) p
  | otherwise = (n, fs)
  where (q, r)    = n `quotRem` p
        composite = r == 0

-- Inspired by haskell.org
primes :: [Integer]
primes = filterPrimes [2..]
  where filterPrimes (p:xs) =
          p : [x | x <- xs, x `mod` p /= 0]
