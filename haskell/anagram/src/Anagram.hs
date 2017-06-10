module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (isAnagram xs)

isAnagram :: String -> String -> Bool
isAnagram xs ys = xhash == yhash && xs' /= ys'
  where xhash = sort xs'
        yhash = sort ys'
        xs' = map toLower xs
        ys' = map toLower ys
