module Series (largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (tails)
import Control.Monad ((>=>))

largestProduct :: Int -> String -> Maybe Integer
largestProduct size = maybeDigits >=> windows size >=> maxProduct' size

maxProduct :: [[Integer]] -> Integer
maxProduct = maximum . map product

maxProduct' :: Int -> [[Integer]] -> Maybe Integer
maxProduct' 0 [] = Just 1
maxProduct' _ [] = Nothing
maxProduct' _ xs = Just (maxProduct xs)

-- See comment on https://stackoverflow.com/a/27733778
windows :: Int -> [a] -> Maybe [[a]]
windows m
  | m < 0     = const Nothing
  | m == 0    = const (Just [])
  | otherwise = Just . foldr (zipWith (:)) (repeat []) . take m . tails

maybeDigit :: Char -> Maybe Integer
maybeDigit c = if isDigit c
               then Just . fromIntegral . digitToInt $ c
               else Nothing

maybeDigits :: String -> Maybe [Integer]
maybeDigits = foldr acc (Just [])
  where acc c = ((:) <$> maybeDigit c <*>)