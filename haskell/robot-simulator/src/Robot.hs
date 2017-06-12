module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
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

simulate :: Robot -> String -> Robot
simulate = foldl instruction

instruction :: Robot -> Char -> Robot
instruction r c = case c of 'L' -> iLeft r
                            'R' -> iRight r
                            'A' -> iAdvance r
                            _   -> r
iLeft :: Robot -> Robot
iLeft (Robot b c) = Robot (turnLeft b) c

iRight :: Robot -> Robot
iRight (Robot b c) = Robot (turnRight b) c

iAdvance :: Robot -> Robot
iAdvance (Robot b c) = Robot b (advance b c)
