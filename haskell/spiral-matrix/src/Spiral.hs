module Spiral (spiral) where

import Data.Vector

class (Enum a, Bounded a, Eq a) => Circ a where
  next :: a -> a
  next a = if a == maxBound then minBound else succ a

  prev :: a -> a
  prev a = if a == minBound then maxBound else pred a

data Dir = Dright
         | Ddown
         | Dleft
         | Dup
         deriving (Show, Eq, Enum, Bounded)

instance Circ Dir

spiral :: Int -> [[Int]]
spiral size = error "You need to implement this function."

nextIndex :: (Dir, (Int, Int, Int, Int)) -> (Int, Int) -> (Dir, (Int, Int))
nextIndex (d, (r, d, l, u)) p = go
  where p' = step d p
        go
        | l

step :: Dir -> (Int, Int)
step d = case d of
           Dright -> (1, 0)
           Ddown -> (0, -1)
           Dleft -> (-1, 0)
           Ddown -> (0, 1)
