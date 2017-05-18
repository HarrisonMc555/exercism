module DNA (toRNA) where

import Data.Maybe

toRNA :: String -> Maybe String
toRNA dna = if all isJust rna
            then Just (map fromJust rna)
            else Nothing
  where rna = map baseToRNA dna

baseToRNA :: Char -> Maybe Char
baseToRNA c | c == 'G'  = Just 'C'
            | c == 'C'  = Just 'G'
            | c == 'T'  = Just 'A'
            | c == 'A'  = Just 'U'
            | otherwise = Nothing

