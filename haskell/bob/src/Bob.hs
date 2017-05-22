module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor input | isSilence  input = "Fine. Be that way!"
                  | isYelling  input = "Whoa, chill out!"
                  | isQuestion input = "Sure."
                  | otherwise        = "Whatever."

isYelling input = any isAlpha input && input == map toUpper input

isQuestion input = last (strip input) == '?'

isSilence input = strip input == ""

strip = f . f
  where f = reverse . dropWhile isSpace
