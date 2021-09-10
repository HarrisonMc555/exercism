module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array.IArray (IArray, Ix, Array, array, bounds, inRange, (!), indices)
-- import Data.Maybe (mapMaybe)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)
type Board = Array Coord (Maybe Color)

board5x5 = [ "  B  "
           , " B B "
           , "B W B"
           , " W W "
           , "  W  " ]
board = parseBoard board5x5
coords = connectedEmptyCoords board Set.empty (1, 1)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories rows =
  let board = parseBoard rows
      coords = Set.fromList $ indices board
      emptyCoords = Set.filter isEmpty coords
      isEmpty coord = board `at` coord == Just Nothing
  in Set.toList $ mapMaybeSet (territoryForBoard board) coords
  -- in []

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor rows = territoryForBoard (parseBoard rows)
-- territoryFor rows coord =
  -- let board = parseBoard rows
  -- in case board `at` coord of
  --   -- In range but no color
  --   Just Nothing ->
  --     let coords = connectedEmptyCoords board Set.empty coord
  --         color = colorForRegion board coords
  --     in Just (coords, color)
  --   _ -> Nothing

territoryForBoard :: Board -> Coord -> Maybe (Set Coord, Maybe Color)
territoryForBoard board coord =
  case board `at` coord of
    -- In range but no color
    Just Nothing ->
      let coords = connectedEmptyCoords board Set.empty coord
          color = colorForRegion board coords
      in Just (coords, color)
    _ -> Nothing

connectedEmptyCoords :: Board -> Set Coord -> Coord -> Set Coord
connectedEmptyCoords board territory coord
  | coord `Set.member` territory = territory
  | not coordIsBlank = territory
  | otherwise =
      let newTerritory = Set.insert coord territory
      in foldl (connectedEmptyCoords board) newTerritory (neighbors coord)
  where coordIsBlank = case board `at` coord of
                         Just Nothing -> True
                         _ -> False

colorForRegion :: Board -> Set Coord -> Maybe Color
colorForRegion board coords =
  let neighborCoords = concatMapSet neighbors coords
      neighborColorsSet = mapMaybeSet id $ mapMaybeSet (board `at`) neighborCoords
      neighborColorsList = Set.toList neighborColorsSet
  in case neighborColorsList of
    [color] -> Just color
    _ -> Nothing

neighbors :: Coord -> [Coord]
neighbors (x, y) =
  [ (x + 1, y)
  , (x - 1, y)
  , (x, y + 1)
  , (x, y - 1)
  ]

parseBoard :: [String] -> Board
parseBoard = fmap parseColor . listToArray

parseColor :: Char -> Maybe Color
parseColor 'B' = Just Black
parseColor 'W' = Just White
parseColor _ = Nothing

-- fmap2d :: (a -> b) -> [[a]] -> [[b]]
-- fmap2d f = fmap (fmap f)

concatMapSet :: (Ord b, Foldable t1, Foldable t2) =>
  (a -> t1 b) -> t2 a -> Set b
concatMapSet f = foldr addAll Set.empty
  where addAll a s = foldr Set.insert s (f a)

mapMaybeSet :: Ord b => (a -> Maybe b) -> Set a -> Set b
mapMaybeSet f = Set.foldr addMaybe Set.empty
  where addMaybe x s = case f x of
                         Just y -> Set.insert y s
                         Nothing -> s

listToArray :: [[a]] -> Array Coord a
listToArray [] = array ((1, 1), (1, 1)) []
listToArray rows@(firstRow:_) =
  let numRows = length rows
      numColumns = length firstRow
  in array ((1, 1), (numColumns, numRows)) $ concat $ indices2d rows

indices2d :: [[a]] -> [[(Coord, a)]]
indices2d = zipWith combine [1..]
  where combine y = zipWith (combine' y) [1..]
        combine' y x a = ((x, y), a)

at :: Ix i => Array i e -> i -> Maybe e
at a i = if bounds a `inRange` i then Just (a ! i) else Nothing
