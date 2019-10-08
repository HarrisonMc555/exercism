module Yacht (yacht, Category(..)) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Arrow ((>>>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Die = Int
type Dice = [Die]
type Score = Int
type Scorer = Dice -> Score

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> Dice -> Score
yacht category =
  case category of
    Ones -> scoreNum 1
    Twos -> scoreNum 2
    Threes -> scoreNum 3
    Fours -> scoreNum 4
    Fives -> scoreNum 5
    Sixes -> scoreNum 6
    FullHouse -> scoreFullHouse
    FourOfAKind -> scoreNOfAKind 4
    LittleStraight -> scoreLittleStraight
    BigStraight -> scoreBigStraight
    Choice -> scoreChoice
    Yacht -> scoreYacht

scoreNum :: Die -> Scorer
scoreNum num = filter (== num) >>> sum

scoreFullHouse :: Scorer
scoreFullHouse dice =
  let diceCounts = counts dice & Map.elems & sort
  in if diceCounts == [2, 3] then sum dice else 0
     
scoreLittleStraight, scoreBigStraight :: Scorer
scoreLittleStraight = scoreMatchingDice [1,2,3,4,5] littleStrightScore
scoreBigStraight = scoreMatchingDice [2,3,4,5,6] bigStraightScore

scoreChoice :: Scorer
scoreChoice dice = sum dice

scoreYacht :: Scorer
scoreYacht dice =
  let firstMaybe = listToMaybe dice
      allMatchMaybe = firstMaybe <&> flip allMatch dice
  in case allMatchMaybe of
    Just True -> yachtScore
    _ -> 0

littleStrightScore, bigStraightScore, yachtScore :: Score
littleStrightScore = 30
bigStraightScore = 30
yachtScore = 50

scoreMatchingDice :: Dice -> Score -> Scorer
scoreMatchingDice matchDice score dice =
  if sort dice == matchDice then score else 0

scoreNOfAKind :: Int -> Scorer
scoreNOfAKind n dice =
  let diceWithCounts = counts dice & Map.assocs
      countIsAtLeastN (_, c) = c >= n
      dieWithAtLeastN = diceWithCounts & filter countIsAtLeastN & map fst & listToMaybe
      scoreMaybe = dieWithAtLeastN <&> (* n)
  in fromMaybe 0 scoreMaybe

counts :: Ord a => [a] -> Map a Int
counts =
  let insert die set = Map.insertWith (+) die 1 set
  in foldr insert Map.empty

allMatch :: Eq a => a -> [a] -> Bool
allMatch a = map (== a) >>> and
