module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite list@(first:_) =
  let regularVerses = map (uncurry verse) . pairs $ list
      lastVerse = firstVerse first
  in intercalate "\n" (regularVerses ++ [lastVerse])
recite [] = ""

firstVerse :: String -> String
firstVerse item = "And all for the want of a " ++ item ++ "."

verse :: String -> String -> String
verse first second =
  "For want of a " ++ first ++ " the " ++ second ++ " was lost."

pairs :: [a] -> [(a, a)]
pairs (x1:rest@(x2:_)) = (x1, x2) : pairs rest
pairs _ = []
