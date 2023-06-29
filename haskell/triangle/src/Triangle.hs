module Triangle (TriangleType(..), triangleType) where

import Data.List (sort)

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | x + y <= z       = Illegal
  | x == y && y == z = Equilateral
  | x == y || y == z = Isosceles
  | otherwise        = Scalene
  where (x, y, z) = sort3 (a, b, c)

sort3 :: (Num a, Ord a) => (a, a, a) -> (a, a, a)
sort3 (a, b, c) =
  case sort [a, b, c] of
    [x, y, z] -> (x, y, z)
    _ -> error "unreachable"