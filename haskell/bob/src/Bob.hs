module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor input | isSilence input = "Fine. Be that way!"
                  | isYellingQuestion input = "Calm down, I know what I'm doing!"
                  | isYelling input = "Whoa, chill out!"
                  | isQuestion input = "Sure."
                  | otherwise = "Whatever."

isYellingQuestion :: String -> Bool
isYellingQuestion input = isYelling input && isQuestion input

isYelling :: String -> Bool
isYelling input = any isAlpha input && input == map toUpper input

isQuestion :: String -> Bool
isQuestion input = last (strip input) == '?'

isSilence :: String -> Bool
isSilence input = strip input == ""

strip :: String -> String
strip = f . f
  where f = reverse . dropWhile isSpace