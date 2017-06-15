module PrimeFactors (primeFactors) where

import Data.List (find)
import Data.Maybe (fromJust)

primeFactors :: Integer -> [Integer]
primeFactors n = snd . fromJust $ find done $ scanl factors (n, []) primes
  where done = (== 1) . fst
        -- problem: doesn't deal with repeated factors
        factors (n, fs) p =
          let (q, r) = quotRem n p
          in if r == 0
             then (q, p:fs)
             else (n, fs)


primes :: [Integer]
primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]
