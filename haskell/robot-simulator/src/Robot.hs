module Robot
    ( Bearing(..)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

-- https://stackoverflow.com/a/20294331/7343786
class (Enum a, Bounded a, Eq a) => Circ a where
  next :: a -> a
  next a = if a == maxBound then minBound else succ a

  prev :: a -> a
  prev a = if a == minBound then maxBound else pred a

data Bearing = North
             | East
             | South
             | West
             deriving (Enum, Bounded, Eq, Show)

instance Circ Bearing

data Robot = Robot { bearing     :: Bearing
                   , coordinates :: Coordinates
                   } deriving (Eq, Show)

type Coordinates = (Integer, Integer)

mkRobot :: Bearing -> Coordinates -> Robot
mkRobot = Robot

turnLeft :: Bearing -> Bearing
turnLeft = prev

turnRight :: Bearing -> Bearing
turnRight = next

advance :: Bearing -> Coordinates -> Coordinates
advance b (x, y) = case b of North -> (x, y+1)
                             East  -> (x+1, y)
                             South -> (x, y-1)
                             West  -> (x-1, y)

move :: Robot -> String -> Robot
move = foldl instruction

instruction :: Robot -> Char -> Robot
instruction r c = case c of 'L' -> iLeft r
                            'R' -> iRight r
                            'A' -> iAdvance r
                            _   -> r
iLeft :: Robot -> Robot
iLeft r@(Robot b _) = r { bearing = turnLeft b }

iRight :: Robot -> Robot
iRight r@(Robot b _) = r { bearing = turnRight b }

iAdvance :: Robot -> Robot
iAdvance r@(Robot b c) = r { coordinates = advance b c }