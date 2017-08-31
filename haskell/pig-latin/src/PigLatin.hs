module PigLatin (translate) where

translate :: String -> String
translate "" = ""
translate cc@(c:_) = if isVowel c
                      then cc ++ pigLatinSuffix
                      else rest ++ beg ++ pigLatinSuffix
  where (beg, rest) = beginningSound cc

pigLatinSuffix :: String
pigLatinSuffix = "ay"

beginningSound :: String -> (String, String)
beginningSound s = if take 2 s == "ch"
                   then ("ch", drop 2 s)
                   else splitAt 1 s

startsWithVowel :: String -> Bool
startsWithVowel "" = False
startsWithVowel (c:_) = isVowel c

isVowel :: Char -> Bool
isVowel = (`elem` vowels)

vowels :: String
vowels = "aeiou"
