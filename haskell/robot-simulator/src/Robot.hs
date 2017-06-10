module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot Bearing Coordinates deriving (Show)

type Coordinates = (Integer, Integer)

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ c) = c

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate = foldl instruction

turnLeft :: Bearing -> Bearing
turnLeft direction = case direction of North -> West
                                       East  -> North
                                       South -> East
                                       West  -> South

turnRight :: Bearing -> Bearing
turnRight direction = case direction of North -> East
                                        East  -> South
                                        South -> West
                                        West  -> North

advance :: Bearing -> Coordinates -> Coordinates
advance b (x, y) = case b of North -> (x, y+1)
                             East  -> (x+1, y)
                             South -> (x, y-1)
                             West  -> (x-1, y)

iLeft :: Robot -> Robot
iLeft (Robot b c) = Robot (turnLeft b) c

iRight :: Robot -> Robot
iRight (Robot b c) = Robot (turnRight b) c

iAdvance :: Robot -> Robot
iAdvance (Robot b c) = Robot b (advance b c)

instruction :: Robot -> Char -> Robot
instruction r c = case c of 'L' -> iLeft r
                            'R' -> iRight r
                            'A' -> iAdvance r
                            _   -> r
