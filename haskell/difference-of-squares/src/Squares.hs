module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSums n - sumOfSquares n

squareOfSums :: Integral a => a -> a
squareOfSums n = square (n*(n+1) `div` 2)

sumOfSquares :: Integral a => a -> a
sumOfSquares n = n*(n+1)*(2*n + 1) `div` 6

_squareOfSums :: Integral a => a -> a
_squareOfSums n = square $ sum [1..n]

_sumOfSquares :: Integral a => a -> a
_sumOfSquares n = sum $ map square [1..n]

square :: Integral a => a -> a
square x = x ^ (2 :: Integer)