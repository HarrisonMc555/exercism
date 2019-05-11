module Poker (bestHands) where

import Data.Char (toUpper)
import Data.Foldable (foldr, find, null)
import Data.Map.Strict as Map (empty, insertWith, toList)
import Data.List (sort, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.Ord (Down(Down))

data Suit = Diamond
          | Heart
          | Spade
          | Club
          -- deriving (Show, Eq, Ord)
          deriving (Eq, Ord)

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
          -- deriving (Show, Eq, Ord, Enum)
          deriving (Eq, Ord, Enum)

data Card = Card { suit :: Suit
                 , rank :: Rank
                 }
          -- deriving (Show, Eq, Ord)
          deriving (Eq, Ord)

type Hand = [Card]

data Score = Score { partialScore :: PartialScore
                   , remainingHand :: Hand
                   }
             deriving (Show)

instance Eq Score where
  (Score part1 rem1) == (Score part2 rem2) =
    part1 == part2 && (rem1 == rem2 || score rem1 == score rem2)

instance Ord Score where
  compare (Score part1 rem1) (Score part2 rem2) =
    case compare part1 part2 of
      EQ -> if rem1 == rem2 then EQ else compare (score rem1) (score rem2)
      other -> other

data PartialScore = None
                  | HighCard Rank
                  | Pair Rank
                  | TwoPair Rank Rank -- High, low
                  | ThreeOfAKind Rank
                  | Straight Rank -- Highest card
                  | Flush [Rank] -- Highest to lowest
                  | FullHouse Rank Rank -- High, low
                  | FourOfAKind Rank
                  | StraightFlush Rank -- Highest card
                  | RoyalFlush
                  deriving (Show, Eq, Ord)

bestHands :: [String] -> Maybe [String]
bestHands handStrings = do hands <- mapM addHand handStrings
                           let handsWithScores = map addScore hands
                           let scores = map thd3 handsWithScores
                           bestScore <- listToMaybe . sortOn Down $ scores
                           let best =
                                 filter ((== bestScore) . thd3) handsWithScores
                           let bestStrings = map fst3 best
                           Just bestStrings
  where addScore (string, hand) = (string, hand, score hand)
        addHand string = do hand <- maybeReadHand string
                            Just (string, hand)
        thd3 (_, _, c) = c
        fst3 (a, _, _) = a

score :: Hand -> Score
score hand = let bestScore = firstJusts . map ($ hand) $ handScorers
                 defaultScore = Score { partialScore = None
                                      , remainingHand = hand
                                      }
             in fromMaybe defaultScore bestScore

handScorers :: [Hand -> Maybe Score]
handScorers = [ royalFlush
              , straightFlush
              , fourOfAKind
              , fullHouse
              , flush
              , straight
              , threeOfAKind
              , twoPair
              , pair
              , highCard
              ]

-- Scorers
royalFlush :: Hand -> Maybe Score
royalFlush hand = do highRank <- highestRankWhenInARow hand
                     let isRoyalFlush = allSameSuit hand && highRank == Ace
                     if isRoyalFlush
                       then Just $ Score { partialScore = RoyalFlush
                                         , remainingHand = []
                                         }
                       else Nothing

straightFlush :: Hand -> Maybe Score
straightFlush hand = do highRank <- highestRankWhenInARow hand
                        let isStraightFlush = allSameSuit hand
                        if isStraightFlush
                          then Just $
                               Score { partialScore = StraightFlush highRank
                                     , remainingHand = []
                                     }
                          else Nothing


fourOfAKind :: Hand -> Maybe Score
fourOfAKind hand = do rankWithFour <- nOfAKind 4 hand
                      let handWithoutRankWithFour =
                            removeNMatching 4 ((== rankWithFour) . rank) hand
                      Just $ Score { partialScore = FourOfAKind rankWithFour
                                   , remainingHand = handWithoutRankWithFour
                                   }


fullHouse :: Hand -> Maybe Score
fullHouse hand = do rankWithThree <- nOfAKind 3 hand
                    rankWithTwo <- nOfAKind 2 hand
                    Just $ Score
                      { partialScore = FullHouse rankWithThree rankWithTwo
                      , remainingHand = []
                      }


flush :: Hand -> Maybe Score
flush hand = do let isFull = isFullHand hand
                let isFlush = isFull && allSameSuit hand
                let ranks = sortOn Down $ map rank hand
                if isFlush
                  then Just $ Score { partialScore = Flush ranks
                                    , remainingHand = []
                                    }
                  else Nothing

straight :: Hand -> Maybe Score
straight hand = do highRank <- highestRankWhenInARow hand
                   Just $ Score { partialScore = Straight highRank
                                , remainingHand = []
                                }

threeOfAKind :: Hand -> Maybe Score
threeOfAKind hand = do rankWithThree <- nOfAKind 3 hand
                       let handWithoutRankWithThree =
                             removeNMatching 3 ((== rankWithThree) . rank) hand
                       Just $ Score { partialScore = ThreeOfAKind rankWithThree
                                    , remainingHand = handWithoutRankWithThree
                                    }

twoPair :: Hand -> Maybe Score
twoPair hand = do rank1 <- nOfAKind 2 hand
                  let handWithoutRank1 =
                        removeNMatching 2 ((== rank1) . rank) hand
                  rank2 <- nOfAKind 2 handWithoutRank1
                  let handWithoutPairs =
                        removeNMatching 2 ((== rank2) . rank) handWithoutRank1
                  let (high, low) = orderPair rank1 rank2
                  Just $ Score { partialScore = TwoPair high low
                               , remainingHand = handWithoutPairs
                               }

pair :: Hand -> Maybe Score
pair hand = do pairRank <- nOfAKind 2 hand
               let handWithoutPair =
                     removeNMatching 2 ((== pairRank) . rank) hand
               Just $ Score { partialScore = Pair pairRank
                            , remainingHand = handWithoutPair
                            }

highCard :: Hand -> Maybe Score
highCard hand = do highRank <- maximumMay $ map rank hand
                   let handWithoutHighRank = filter ((/= highRank) . rank) hand
                   Just $ Score { partialScore = HighCard highRank
                                , remainingHand = handWithoutHighRank
                                }

-- Poker helpers
highestRankWhenInARow :: Hand -> Maybe Rank
highestRankWhenInARow hand =
  let isFull = isFullHand hand
      inRow = isAllInRow hand
  in if isFull && inRow
     then maximumMay $ map rank hand
     else Nothing

allSameSuit :: Hand -> Bool
allSameSuit = allEqual . map suit

nOfAKind :: Int -> Hand -> Maybe Rank
nOfAKind num hand = let ranks = map rank hand
                        countedRanks = counts ranks
                        containsNum = (== num) . snd
                    in fst <$> find containsNum countedRanks

isFullHand :: Hand -> Bool
isFullHand hand = length hand == 5

isAllInRow :: Hand -> Bool
isAllInRow = isSucc . sort . map rank

-- Generic helpers
firstJusts :: [Maybe a] -> Maybe a
firstJusts = listToMaybe . catMaybes

counts :: (Foldable f, Ord a, Eq a) => f a -> [(a, Int)]
counts = toList . foldr insertOrInc empty
  where insertOrInc key = insertWith (+) key 1

maximumMay :: (Foldable f, Ord a) => f a -> Maybe a
maximumMay xs
  | null xs = Nothing
  | otherwise = Just $ maximum xs

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs


isSucc :: (Enum a, Eq a) => [a] -> Bool
isSucc [] = True
isSucc [_] = True
isSucc (x:y:zs) | y == succ x = isSucc $ y:zs
isSucc _ = False

orderPair :: (Ord a) => a -> a -> (a, a)
orderPair x y = (big, small)
  where big = max x y
        small = min x y

removeNMatching :: Int -> (a -> Bool) -> [a] -> [a]
removeNMatching 0 _ xs = xs
removeNMatching _ _ [] = []
removeNMatching num f (x:xs) = if f x
                               then removeNMatching (num-1) f xs
                               else x : removeNMatching num f xs

-- Reading
maybeReadHand :: String -> Maybe Hand
maybeReadHand = mapM maybeReadCard . words

maybeReadCard :: String -> Maybe Card
maybeReadCard input = do (rankString, suitChar) <- splitLast input
                         suit' <- maybeReadSuit suitChar
                         rank' <- maybeReadRank rankString
                         return $ Card suit' rank'

maybeReadSuit :: Char -> Maybe Suit
maybeReadSuit c = case toUpper c of
                    'D' -> Just Diamond
                    'H' -> Just Heart
                    'S' -> Just Spade
                    'C' -> Just Club
                    _   -> Nothing

maybeReadRank :: String -> Maybe Rank
maybeReadRank s = case s of
                    "2"  -> Just Two
                    "3"  -> Just Three
                    "4"  -> Just Four
                    "5"  -> Just Five
                    "6"  -> Just Six
                    "7"  -> Just Seven
                    "8"  -> Just Eight
                    "9"  -> Just Nine
                    "10" -> Just Ten
                    "J"  -> Just Jack
                    "Q"  -> Just Queen
                    "K"  -> Just King
                    "A"  -> Just Ace
                    _    -> Nothing

splitLast :: [a] -> Maybe ([a], a)
splitLast [] = Nothing
splitLast xs = Just $ splitLastHelper xs
  where splitLastHelper [] = error "splitLastHelper called with empty list"
        splitLastHelper [x] = ([], x)
        splitLastHelper (x:xs') = let (first, end) = splitLastHelper xs'
                                  in (x:first, end)

prettyCard :: Card -> String
prettyCard (Card s r) = prettySuit s ++ prettyRank r

prettySuit :: Suit -> String
prettySuit s = case s of
                 Diamond -> "D"
                 Heart -> "H"
                 Spade -> "S"
                 Club -> "C"

prettyRank :: Rank -> String
prettyRank r = case r of
                 Two -> "2"
                 Three -> "3"
                 Four -> "4"
                 Five -> "5"
                 Six -> "6"
                 Seven -> "7"
                 Eight -> "8"
                 Nine -> "9"
                 Ten -> "10"
                 Jack -> "J"
                 Queen -> "Q"
                 King -> "K"
                 Ace -> "A"

instance Show Suit where
  show = prettySuit

instance Show Rank where
  show = prettyRank

instance Show Card where
  show = prettyCard
