module Acronym (abbreviate) where

import Data.Char (isAlphaNum, toUpper)

abbreviate :: String -> String
abbreviate xs = map toUpper $ map head $ wordsNoSymbols xs

wordsNoSymbols :: String -> [String]
wordsNoSymbols = map (filter isAlphaNum) . words

-- This was my first try
split :: (a -> Bool) -> [a] -> [[a]]
split f [] = []
split f xs = if hasThis
             then [this] ++ (split f next)
             else []
  where this = takeWhile f xs'
        next = dropWhile f xs'
        hasThis = not $ null this
        xs'  = dropWhile (not . f) xs
