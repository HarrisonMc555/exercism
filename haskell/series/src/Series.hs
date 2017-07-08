module Series (slices) where

import Control.Arrow ((>>>))
import Data.Maybe (fromMaybe)
import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n = intList >>> windows n >>> fromMaybe []

intList :: String -> [Int]
intList = map (read . pure)

windows :: Int -> [a] -> Maybe [[a]]
windows m
  | m < 0     = const Nothing
  | m == 0    = const (Just [[]])
  | otherwise = Just . foldr (zipWith (:)) (repeat []) . take m . tails
