module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p = keep (not . p)

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep f (x:xs)
  | f x       = x : rest
  | otherwise = rest
  where rest = keep f xs
