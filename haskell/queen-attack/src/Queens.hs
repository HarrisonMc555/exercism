module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines [ rowString row | row <- grid ]
  where rowString = unwords . map queenString
        queenString pos
          | white `maybeEq` pos = "W"
          | black `maybeEq` pos = "B"
          | otherwise = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (aX, aY) (bX, bY)
  | aX == bX = True
  | aY == bY = True
  | yDiff == xDiff = True
  | yDiff == -xDiff = True
  | otherwise = False
  where xDiff = bX - aX
        yDiff = bY - aY

iRows :: [Int]
iRows = [0..7]

iCols :: [Int]
iCols = [0..7]

grid :: [[(Int, Int)]]
grid = [ [ (iRow, iCol) | iCol <- iCols ] | iRow <- iRows ]

maybeEq :: (Eq a) => Maybe a -> a -> Bool
maybeEq Nothing _ = False
maybeEq (Just x) y = x == y