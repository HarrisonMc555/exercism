module Pangram (isPangram) where

import Data.Char

isPangram :: String -> Bool
isPangram text = all textContains ['a'..'z']
  where textContains c = elem c loweredText
        loweredText = map toLower text
