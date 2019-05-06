module Transpose (transpose) where

import Data.List (tails)

transpose :: [String] -> [String]
transpose = transposeDefault ' '

transposeDefault :: a -> [[a]] -> [[a]]
transposeDefault def = transpose' . padListsDescending def

transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' ([] : xss) = transpose' xss
transpose' ((x:xs) : xss) =
  let firstRow = x : [h | (h:_) <- xss]
      nextRows = transpose' (xs : [t | (_:t) <- xss])
  in firstRow : nextRows

padRight :: a -> Int -> [a] -> [a]
padRight x n xs = take n $ xs ++ repeat x

padLists :: a -> [[a]] -> [[a]]
padLists x xss = let n = maximum [length xs | xs <- xss]
                 in [padRight x n xs | xs <- xss]

padListsDescending :: a -> [[a]] -> [[a]]
padListsDescending _ [] = []
padListsDescending x xss =
  let lengths = [length xs | xs <- xss]
      pad xs ls = padRight x (maximum ls) xs
  in zipWith pad xss (tails lengths)
