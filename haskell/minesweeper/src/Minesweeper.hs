module Minesweeper (annotate) where

import Data.Ix (Ix, range, inRange)
import Data.Array (Array, (!), listArray, bounds, assocs, array)
import Data.Char (intToDigit)

data Tile = Mine
          | Empty Int -- Number of mines next to (0-8)
          deriving Show
data UTile = UMine
           | UEmpty
           deriving Show

type Board  = Array (Int, Int) Tile
type UBoard = Array (Int, Int) UTile

annotate :: [String] -> [String]
annotate = boardToStrings . stringsToBoard

stringsToBoard :: [String] -> Array (Int, Int) Tile
stringsToBoard = uBoardToBoard . stringsToUBoard

boardToStrings :: Board -> [String]
boardToStrings = array2dToLists . fmap tileToChar

tileToChar :: Tile -> Char
tileToChar Mine = '*'
tileToChar (Empty n)
  | n == 0    = ' '
  | otherwise = intToDigit n

uBoardToBoard :: UBoard -> Board
uBoardToBoard ub = let f = uTileToTile ub
                   in mapWithIndex f ub

stringsToUBoard :: [String] -> UBoard
stringsToUBoard css =
  let utss  = stringsToUTileList css
      nrows = length utss
      ncols = if nrows == 0
              then 0
              else length (head utss)
  in if isRectangular css
     then listArray ((1, 1), (nrows, ncols)) (concat utss)
     else error "Non-rectangular board"

stringsToUTileList :: [String] -> [[UTile]]
stringsToUTileList = map (map charToUTile)

uTileToTile :: UBoard -> (Int, Int) -> UTile -> Tile
uTileToTile _ _ UMine  = Mine
uTileToTile ub i UEmpty = Empty $ numUMinesNextTo ub i

numUMinesNextTo :: UBoard -> (Int, Int) -> Int
numUMinesNextTo  = go
  where go ub = length . filter isUMine . map (ub !) .
                neighboringInBoundIndices ub

isUMine :: UTile -> Bool
isUMine UMine  = True
isUMine UEmpty = False

charToUTile :: Char -> UTile
charToUTile '*' = UMine
charToUTile ' ' = UEmpty
charToUTile _   = error "Invalid char"

mapWithIndex :: Ix i => (i -> x -> y) -> Array i x -> Array i y
mapWithIndex f a = let b = bounds a
                   in array b [ (i, f i e) | (i, e) <- assocs a]

neighboringInBoundIndices :: Array (Int, Int) a -> (Int, Int) -> [(Int, Int)]
neighboringInBoundIndices a = let b  = bounds a
                              in filter (inRange b) . neighboringIndices

neighboringIndices :: (Int, Int) -> [(Int, Int)]
neighboringIndices (r, c) = [ (r', c')
                            | r' <- [r-1..r+1]
                            , c' <- [c-1..c+1]
                            , (r', c') /= (r, c)]

array2dToLists :: Array (Int, Int) e -> [[e]]
array2dToLists a = let ((lr, lc), (hr, hc)) = bounds a
                   in [ [ a ! (r, c) | c <- range (lc, hc)]
                      | r <- range (lr, hr)]

isRectangular :: [[a]] -> Bool
isRectangular [] = True
isRectangular xss = let ncols = length (head xss)
                    in all ((ncols ==) . length) xss
