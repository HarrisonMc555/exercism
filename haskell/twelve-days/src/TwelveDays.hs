module TwelveDays (recite) where

import Data.List (intercalate)

recite :: Int -> Int -> [String]
recite start stop = map day [start..stop]

gift :: Int -> String
gift n = case n of
  1 -> "a Partridge in a Pear Tree"
  2 -> "two Turtle Doves"
  3 -> "three French Hens"
  4 -> "four Calling Birds"
  5 -> "five Gold Rings"
  6 -> "six Geese-a-Laying"
  7 -> "seven Swans-a-Swimming"
  8 -> "eight Maids-a-Milking"
  9 -> "nine Ladies Dancing"
  10 -> "ten Lords-a-Leaping"
  11 -> "eleven Pipers Piping"
  12 -> "twelve Drummers Drumming"
  _  -> error "invalid day of Christmas"

ordinal :: Int -> String
ordinal n = case n of
  1 -> "first"
  2 -> "second"
  3 -> "third"
  4 -> "fourth"
  5 -> "fifth"
  6 -> "sixth"
  7 -> "seventh"
  8 -> "eighth"
  9 -> "ninth"
  10 -> "tenth"
  11 -> "eleventh"
  12 -> "twelfth"
  _  -> error "invalid day of Christmas"

day :: Int -> String
day n = let o = ordinal n
        in ("On the " ++ o ++ " day of Christmas my true love gave to me, " ++
            gifts n ++ ".")

gifts :: Int -> String
gifts n
  | n == 1 = gift 1
  | n > 1 && n <= 12 = let r = [n, n-1..2]
                       in intercalate ", " (map gift r) ++ ", and " ++ gift 1
  | otherwise = error "invalid day of Christmas"