module Poker (bestHands) where

import           Data.Char (toUpper)

data Suit = Diamond
          | Heart
          | Spade
          | Club
          deriving (Show, Eq, Ord)

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
          deriving (Show, Eq, Ord)

data Card = Card { suit :: Suit
                 , rank :: Rank
                 }
          deriving (Show, Eq, Ord)

type Hand = [Card]

data Score = Score { partialScore :: PartialScore
                   , remainingHand :: Hand
                   }
             deriving (Show, Eq)

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

bestHands :: [String] -> Maybe [String]
bestHands hands = do let _cards = map maybeReadHand hands
                     error "You need to implement this function!"

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
