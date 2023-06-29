module Sieve (primesUpTo) where

import Data.List ((\\))

primesUpTo :: Integer -> [Integer]
primesUpTo n = takeWhile (<= n) $ filterPrimes [2..n]
  where filterPrimes (p:xs) = p : filterPrimes [x | x <- xs, x `mod` p /= 0]
        filterPrimes [] = []
        

_primesUpTo :: Integer -> [Integer]
_primesUpTo n = sieve [2..n]
  where sieve (x:xs) = x : sieve (xs \\ [x,x+x..n])
        sieve [] = []