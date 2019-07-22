module Connect (Mark(..), winner) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array (Array, listArray, bounds, (!))
import Data.Ix (inRange)
import Data.Maybe (mapMaybe, listToMaybe)
import Control.Monad (guard)

-- Cross goes from left to right, Nought goes from top to bottom
data Mark = Cross | Nought deriving (Eq, Show)
data Spot = Filled Mark | Empty deriving (Eq, Show)
type Board = Array Location Spot
type Location = (Int, Int)

winner :: [String] -> Maybe Mark
winner rowStrings = do board <- parseBoard rowStrings
                       case (crossWins board, noughtWins board) of
                         (True, True) -> Nothing
                         (True, False) -> Just Cross
                         (False, True) -> Just Nought
                         (False, False) -> Nothing
                         
crossWins :: Board -> Bool
crossWins board =
  let reachable = reachableFrom board Cross (crossStartingLocations board)
      ending = Set.fromList $ crossEndingLocations board
  in any (`Set.member` ending) reachable

noughtWins :: Board -> Bool
noughtWins board =
  let reachable = reachableFrom board Nought (noughtStartingLocations board)
      ending = Set.fromList $ noughtEndingLocations board
  in any (`Set.member` ending) reachable

crossStartingLocations :: Board -> [Location]
crossStartingLocations board =
  let ((rowStart, colStart), (rowEnd, _)) = bounds board
  in [(row, colStart) | row <- [rowStart..rowEnd]]

crossEndingLocations :: Board -> [Location]
crossEndingLocations board =
  let ((rowStart, _), (rowEnd, colEnd)) = bounds board
  in [(row, colEnd) | row <- [rowStart..rowEnd]]

noughtStartingLocations :: Board -> [Location]
noughtStartingLocations board =
  let ((rowStart, colStart), (_, colEnd)) = bounds board
  in [(rowStart, col) | col <- [colStart..colEnd]]

noughtEndingLocations :: Board -> [Location]
noughtEndingLocations board =
  let ((_, colStart), (rowEnd, colEnd)) = bounds board
  in [(rowEnd, col) | col <- [colStart..colEnd]]

reachableFrom :: Board -> Mark -> [Location] -> [Location]
reachableFrom board mark =
  Set.toList . foldr go Set.empty . filter ((== Filled mark) . (board !))
  where go location seen =
          let seen' = Set.insert location seen
              seen'' = foldr go seen' (nextLocations board mark seen location)
          in seen''

nextLocations :: Board -> Mark -> Set Location -> Location -> [Location]
nextLocations board mark seen location =
  [ l | l <- adjacent location
      , inRange (bounds board) l
      , board ! l == Filled mark
      , not $ Set.member l seen ]

adjacent :: Location -> [Location]
adjacent (row, column) = [ (row - 1, column + 0)
                         , (row - 1, column + 1)
                         , (row + 0, column - 1)
                         , (row + 0, column + 1)
                         , (row + 1, column - 1)
                         , (row + 1, column + 0)
                         ]

parseBoard :: [String] -> Maybe Board
parseBoard rowStrings =
  let rows = map (mapMaybe parseMark) rowStrings
  in do firstRow <- listToMaybe rows
        let width = length firstRow
        guard $ all ((== width) . length) rows
        let height = length rows
            bounds' = ((0, 0), (height - 1, width - 1))
            fields = concat rows
        return $ listArray bounds' fields

parseMark :: Char -> Maybe Spot
parseMark 'X' = Just (Filled Cross)
parseMark 'O' = Just (Filled Nought)
parseMark '.' = Just Empty
parseMark _   = Nothing

