module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z (x:xs) =
  let z' = f z x
  in z' `seq` foldl' f z' xs
foldl' _ z [] = z

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z (x:xs) = f x (foldr f z xs)
foldr _ z []     = z

length :: [a] -> Int
length = foldl' inc 0
  where inc n _ = n + 1

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f (x:xs) = f x : map f xs
map _ []     = []

filter :: (a -> Bool) -> [a] -> [a]
filter p (x:xs)
  | p x = x : filter p xs
  | otherwise = filter p xs
filter _ [] = []

(++) :: [a] -> [a] -> [a]
(x:xs) ++ ys = x : xs ++ ys
[] ++ ys     = ys

concat :: [[a]] -> [a]
concat = foldr (++) []
