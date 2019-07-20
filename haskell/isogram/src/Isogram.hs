module Isogram (isIsogram) where

import qualified Data.Set as Set
import Data.Maybe (isJust)
import Data.Char (toLower, isLetter)

isIsogram :: String -> Bool
isIsogram = isJust . foldr acc zero . clean
  where acc c s = s >>= insertMaybe c
        insertMaybe c s = if c `Set.member` s
                          then Nothing
                          else Just (Set.insert c s)
        zero = Just Set.empty

clean :: String -> String
clean = map toLower . filter isLetter