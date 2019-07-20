{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrimeFactors (primeFactors) where

import Data.List (find)
import Data.Maybe (fromJust)
import Data.Numbers.Primes (primes)

primeFactors :: Integer -> [Integer]
primeFactors n = getResult . find done . scanl addFactors (n, []) $ primes
  where done = (1 ==) . fst
        getResult = reverse . snd . fromJust

addFactors :: (Integer, [Integer]) -> Integer -> (Integer, [Integer])
addFactors nfs@(n, fs) p
  | n == 1    = nfs
  | composite = addFactors (q, p:fs) p
  | otherwise = nfs
  where (q, r)    = n `quotRem` p
        composite = r == 0

-- If I use this, the final test (93819012551) takes far too long. I haven't
-- timed it yet, because I'm not sure how long it would take. I waited for 5
-- minutes and it hadn't stopped yet. It only takes 0.34 seconds with the primes
-- package though.
-- -- Inspired by haskell.org
-- primes :: [Integer]
-- primes = filterPrimes [2..]
--   where filterPrimes (p:xs) =
--           p : filterPrimes [x | x <- xs, x `mod` p /= 0]