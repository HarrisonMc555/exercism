module Pangram (isPangram) where

import Data.Char

isPangram :: String -> Bool
isPangram text = all textContains ['a'..'z']
  where textContains = (`elem` loweredText)
        loweredText = map toLower text
