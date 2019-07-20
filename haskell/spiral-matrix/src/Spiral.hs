module Spiral (spiral) where

import Data.List (groupBy, sort)

class (Enum a, Bounded a, Eq a) => Overflow a where
  next :: a -> a
  next a = if a == maxBound then minBound else succ a

data Dir = Dright
         | Ddown
         | Dleft
         | Dup
         deriving (Show, Eq, Enum, Bounded)

instance Overflow Dir

type Pos = (Int, Int)
type Bounds = (Pos, Pos)

spiral :: Int -> [[Int]]
spiral = map (map snd) . groupBy sameRow . sort . unorderedSpiral
  where sameRow a b = getRow a == getRow b
        getRow = fst . fst

unorderedSpiral :: Int -> [(Pos, Int)]
unorderedSpiral size = zipWith extract nums $ iterate getNext start
  where extract i (p, _, _) = (p, i)
        nums = [1 .. sizeSquared]
        sizeSquared = size ^ (2 :: Int)
        start = (startPos, startDir, startBounds)
        startPos = (1, 1)
        startDir = Dright
        startBounds = ((1, size), (1, size))

getNext :: (Pos, Dir, Bounds) -> (Pos, Dir, Bounds)
getNext t = if isEdge t
            then turn t
            else forward t

forward :: (Pos, Dir, Bounds) -> (Pos, Dir, Bounds)
forward (p, d, b) = (step d p, d, b)

turn :: (Pos, Dir, Bounds) -> (Pos, Dir, Bounds)
turn (p, d, b) = (step d' p, d', shrink d' b)
  where d' = next d

isEdge :: (Pos, Dir, Bounds) -> Bool
isEdge ((r, c), d, ((r1, r2), (c1, c2))) = case d of 
  Dright -> c == c2
  Ddown  -> r == r2
  Dleft  -> c == c1
  Dup    -> r == r1

shrink :: Dir -> Bounds -> Bounds
shrink d (rs@(r1,r2), cs@(c1,c2)) = case d of
  Dright -> (rs         , (c1+1, c2))
  Ddown  -> ((r1+1, r2) , cs)
  Dleft  -> (rs         , (c1, c2-1))
  Dup    -> ((r1, r2-1) , cs)

step :: Dir -> Pos -> Pos
step d (r, c) = (r + dr, c + dc)
  where (dr, dc) = inc d

inc :: Dir -> (Int, Int)
inc d = case d of
  Dright -> (0, 1)
  Ddown  -> (1, 0)
  Dleft  -> (0, -1)
  Dup    -> (-1, 0)