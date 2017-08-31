module PigLatin (translate) where

import Data.Maybe (isJust, fromJust)
import Data.List (find, stripPrefix, isPrefixOf)
import Data.Char (isAlpha)

-- Translate a phrase into Pig Latin.
translate :: String -> String
translate = mapWords translateWord

-- Translate a single word into Pig Latin.
translateWord :: String -> String
translateWord s
  | isJust vowelResult   = pigVowel (fromJust vowelResult)
  | isJust consantResult = pigConsonant (fromJust consantResult)
  | otherwise            = s
  where vowelResult   = splitVowels s
        consantResult = splitConsonants s
        pigVowel (pre, rest)     = pre ++ rest ++ pigLatinSuffix
        pigConsonant (pre, rest) = rest ++ pre ++ pigLatinSuffix
        pigLatinSuffix = "ay"

-- Splits word into beginning vowel portion and rest if it starts with vowels.
splitVowels :: String -> Maybe (String, String)
splitVowels = splitLetters vowelClusters isVowel

-- Splits word into beginning consonant portion and rest if it starts with
-- consonants.
splitConsonants :: String -> Maybe (String, String)
splitConsonants s
  | charThenQu = if isConsonant (head prefix) then Just pair else Nothing
  | otherwise  = splitLetters consonantClusters isConsonant s
  where pair@(prefix, _) = splitAt 3 s
        charThenQu = length prefix == 3 && drop 1 prefix == "qu"

-- Splits word into beginning portion then rest as determined by list of
-- prefixes and function to apply to first character.
splitLetters :: [String] -> (Char -> Bool) -> String -> Maybe (String, String)
splitLetters ss f s
  | null s         = Nothing
  | isJust cluster = cluster
  | f c            = Just ([c], cs)
  | otherwise      = Nothing
  where c       = head s
        cs      = drop 1 s
        cluster = find (`isPrefixOf` s) ss >>= flip splitPrefix s

-- Split list into prefix and rest.
splitPrefix :: Eq a => [a] -> [a] -> Maybe ([a], [a])
splitPrefix prefix list = let p = stripPrefix prefix list
  in flip splitAt list . const (length prefix) <$> p

isVowel :: Char -> Bool
isVowel = (`elem` vowels)

isConsonant :: Char -> Bool
isConsonant c = isAlpha c && not (isVowel c)

vowels :: String
vowels = "aeiou"

vowelClusters :: [String]
vowelClusters = [ "yt"
                , "xr"
                ]

consonantClusters :: [String]
consonantClusters = [ "ch"
                    , "qu"
                    , "thr"
                    , "th"
                    , "sch"
                    ]

mapWords :: (String -> String) -> String -> String
mapWords f = unwords . map f . words
