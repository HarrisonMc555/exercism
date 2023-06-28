module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)

abbreviate :: String -> String
abbreviate xs = map toUpper $ concatMap acronymLetters $ split xs

split :: String -> [String]
split s = words $ map nonLetterToSpace s

nonLetterToSpace :: Char -> Char
nonLetterToSpace c
  | isAlpha c = c
  | c == '\'' = c
  | otherwise = ' '

acronymLetters :: String -> String
acronymLetters s
  | all isUpper s = [head s]
  | any isUpper s = filter isUpper s
  | otherwise     = [head s]