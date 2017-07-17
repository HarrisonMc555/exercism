module Prime (nth) where

import Data.Numbers.Primes (primes)

nth :: Int -> Maybe Integer
nth n
  | n <= 0 = Nothing
  | otherwise = Just $ primes !! (n-1)

-- -- Inspired by haskell.org
-- primes :: [Integer]
-- primes = filterPrimes [2..]
--   where filterPrimes (p:xs) =
--           p : filterPrimes [x | x <- xs, x `mod` p /= 0]
