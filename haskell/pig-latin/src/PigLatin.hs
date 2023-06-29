module PigLatin (translate) where

import Data.Maybe (isJust, fromJust)
import Data.List (find, stripPrefix, isPrefixOf)
import Data.Char (isAlpha)

translate :: String -> String
translate = mapWords translateWord

translateWord :: String -> String
translateWord s
  | isJust vowelResult   = pigVowel (fromJust vowelResult)
  | isJust consantResult = pigConsonant (fromJust consantResult)
  | otherwise            = s
  where vowelResult   = splitVowels s
        consantResult = splitConsonants s
        pigVowel (pre, rest)     = pre ++ rest ++ pigLatinSuffix
        pigConsonant (pre, rest) = rest ++ pre ++ pigLatinSuffix

pigLatinSuffix :: String
pigLatinSuffix = "ay"

splitPrefix :: Eq a => [a] -> [a] -> Maybe ([a], [a])
splitPrefix prefix list = let p = stripPrefix prefix list
  in flip splitAt list . const (length prefix) <$> p

splitLetters :: (Char -> Bool) -> [String] -> String -> Maybe (String, String)
splitLetters f ss s
  | null s         = Nothing
  | isJust cluster = cluster
  | f c            = Just ([c], cs)
  | otherwise      = Nothing
  where c       = head s
        cs      = drop 1 s
        cluster = find (`isPrefixOf` s) ss >>= flip splitPrefix s

splitVowels :: String -> Maybe (String, String)
splitVowels = splitLetters isVowel vowelClusters

splitConsonants :: String -> Maybe (String, String)
splitConsonants s
  | thenQu    = if isConsonant (head prefix) then Just pair else Nothing
  | otherwise = splitLetters isConsonant consonantClusters s
  where pair@(prefix, _) = splitAt 3 s
        thenQu = length prefix == 3 && drop 1 prefix == "qu"

mapWords :: (String -> String) -> String -> String
mapWords f = unwords . map f . words

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
                    , "rh"
                    ]