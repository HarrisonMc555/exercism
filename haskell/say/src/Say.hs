module Say (inEnglish) where

import Data.Maybe (fromJust)

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0                    = Nothing
  | n < 10 ^ (1  :: Integer) = Just $ sayOnes n
  | n < 10 ^ (2  :: Integer) = Just $ sayTens n
  | n < 10 ^ (3  :: Integer) = Just $ sayHundreds n
  | n < 10 ^ (6  :: Integer) = Just $ sayThousands n
  | n < 10 ^ (9  :: Integer) = Just $ sayMillions n
  | n < 10 ^ (12 :: Integer) = Just $ sayBillions n
  | otherwise                = Nothing

sayOnes :: Integer -> String
sayOnes n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> error "Called sayOnes with number not 0 to 9 inclusive"

sayTeen :: Integer -> String
sayTeen n = case n of
  10 -> "ten"
  11 -> "eleven"
  12 -> "twelve"
  13 -> "thirteen"
  14 -> "fourteen"
  15 -> "fifteen"
  16 -> "sixteen"
  17 -> "seventeen"
  18 -> "eighteen"
  19 -> "nineteen"
  _ -> error "Called sayTeen with number not 10 to 19 inclusive"

sayTen :: Integer -> String
sayTen n = case n of
  2 -> "twenty"
  3 -> "thirty"
  4 -> "forty"
  5 -> "fifty"
  6 -> "sixty"
  7 -> "seventy"
  8 -> "eighty"
  9 -> "ninety"
  _ -> "Called sayTen with number not 2 to 9 inclusive"

sayTens :: Integer -> String
sayTens n
  | tens == 0 = sayOnes ones
  | tens == 1 = sayTeen n
  | tens < 10 = if ones == 0
                then sayTen tens
                else sayTen tens ++ "-" ++ sayOnes ones
  | otherwise = error "Called sayTens with number not 0 to 9 inclusive"
  where (tens, ones) = n `quotRem` 10

sayHundreds :: Integer -> String
sayHundreds n
  | n < 100 = sayTens n
  | otherwise = let (hundreds, left) = n `quotRem` 100
                in if left == 0
                   then sayOnes hundreds ++ " hundred"
                   else sayOnes hundreds ++ " hundred " ++ sayTens left

sayPowerOfThousand :: Integer -> String -> Integer -> String
sayPowerOfThousand b s n = let (thousands, left) = n `quotRem` (1000 ^ b)
  in if left == 0
     then sayHundreds thousands ++ " " ++ s
     else sayHundreds thousands ++ " " ++ s ++ " " ++ fromJust (inEnglish left)

sayThousands :: Integer -> String
sayThousands = sayPowerOfThousand 1 "thousand"

sayMillions :: Integer -> String
sayMillions = sayPowerOfThousand 2 "million"

sayBillions :: Integer -> String
sayBillions = sayPowerOfThousand 3 "billion"