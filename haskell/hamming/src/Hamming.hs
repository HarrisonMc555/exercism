module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys = if length xs == length ys
                 then Just $ count diff $ zip xs ys
                 else Nothing
  where count f = length . filter f
        diff pair = fst pair /= snd pair
