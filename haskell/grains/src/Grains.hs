module Grains (square, total) where

import Data.Maybe (fromJust)

square :: Integer -> Maybe Integer
square n = if 0 < n && n <= 64
           then Just $ 2 ^ (n - 1)
           else Nothing

total :: Integer
total = sum $ map (fromJust . square) [1..64]