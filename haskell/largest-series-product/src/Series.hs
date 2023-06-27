module Series (largestProduct, Error(..)) where

import Data.Char (isDigit, digitToInt)
import Data.List (tails)
import Control.Monad ((>=>))

data Error = InvalidSpan
           | InvalidDigit Char
           deriving (Eq, Show)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size = parseDigits >=> windows size >=> maxProduct' size

maxProduct :: [[Integer]] -> Integer
maxProduct = maximum . map product

maxProduct' :: Int -> [[Integer]] -> Either Error Integer
maxProduct' 0 [] = Right 1
maxProduct' _ [] = Left InvalidSpan
maxProduct' _ xs = Right (maxProduct xs)

-- See comment on https://stackoverflow.com/a/27733778
windows :: Int -> [a] -> Either Error [[a]]
windows m
  | m < 0     = const $ Left InvalidSpan
  | m == 0    = const $ Right []
  | otherwise = Right . foldr (zipWith (:)) (repeat []) . take m . tails

parseDigit :: Char -> Either Error Integer
parseDigit c =
  fromIntegral <$> digitToIntSafe c

parseDigits :: String -> Either Error [Integer]
parseDigits =
  traverse parseDigit

digitToIntSafe :: Char -> Either Error Int
digitToIntSafe c =
  if isDigit c
  then Right $ digitToInt c
  else Left $ InvalidDigit c
