module BinarySearch (find) where

import Data.Array (Array, bounds, (!))

find :: Ord a => Array Int a -> a -> Maybe Int
find array = findHelper array (bounds array)

findHelper :: Ord a => Array Int a -> (Int, Int) -> a -> Maybe Int
findHelper array (low, high) searchValue
  | high < low = Nothing
  | otherwise = let midIndex = average low high
                    midValue = array ! midIndex
                in case compare searchValue midValue of
                     LT -> findHelper array (low, midIndex - 1) searchValue
                     EQ -> Just midIndex
                     GT -> findHelper array (midIndex + 1, high) searchValue

average :: Int -> Int -> Int
average x y = (x + y) `div` 2
