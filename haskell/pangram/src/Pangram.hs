module Pangram (isPangram) where

import Data.Char

isPangram :: String -> Bool
isPangram text = all textContains ['a'..'z']
  where textContains c = c `elem` loweredText
        loweredText = map toLower text
