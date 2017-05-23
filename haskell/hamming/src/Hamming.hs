module Hamming (distance) where

-- distance :: String -> String -> Maybe Int
-- distance xs ys = if length xs == length ys
--                  then Just $ count diff $ zip xs ys
--                  else Nothing
--   where count f = length . filter f
--         diff pair = fst pair /= snd pair

distance :: String -> String -> Maybe Int
distance [] []         = Just 0
distance (x:xs) (y:ys) = xydiff `plus` distance xs ys
  where xydiff = if x == y then Just 0 else Just 1
        plus (Just a) (Just b) = Just $ a + b
        plus _ _               = Nothing
distance _ _           = Nothing
