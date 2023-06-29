module Sublist (sublist) where

import Prelude hiding ( length
                      , any
                      , map
                      , takeWhile
                      , head)
import Data.Vector ( Vector
                   , length
                   , any
                   , map
                   , takeWhile
                   , head
                   , slice
                   , fromList
                   , elemIndices
                   )

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xInY && yInX = Just EQ
  | xInY         = Just LT
  | yInX         = Just GT
  | otherwise    = Nothing
  where xsV  = fromList xs
        ysV  = fromList ys
        xInY = isSubvector xsV ysV
        yInX = isSubvector ysV xsV


isSubvector :: Eq a => Vector a -> Vector a -> Bool
isSubvector xsV ysV
  | null xsV  = True
  | otherwise = let startings = elemIndices (head xsV) ysV
                    possibles = slices startings (length xsV) ysV
                in any (== xsV) possibles

slices :: Vector Int -> Int -> Vector a -> Vector (Vector a)
slices is n v = map slice' (takeWhile inRange is)
  where slice'  i = slice i n v
        inRange i = len - i >= n
        len       = length v