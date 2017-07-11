module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)

wordCount :: String -> [(String, Int)]
wordCount = foldr addWord [] . cleanWords

addWord :: String -> [(String, Int)] -> [(String, Int)]
addWord word pairs = case lookup word pairs of
  Just _ -> map incWord pairs
  Nothing -> (word, 1):pairs
  where incWord (w, n) = (w, if w == word
                             then n + 1
                             else n)

cleanWords :: String -> [String]
cleanWords = map unquoteWord . words . map (letterCharToSpace . toLower)
  where letterCharToSpace c = if letterChar c
                              then c
                              else ' '

unquoteWord :: String -> String
unquoteWord word
  | length word < 2 = word
  | head word == '\'' && last word == '\'' = (tail . init) word
  | otherwise = word

letterChar :: Char -> Bool
letterChar c = isAlphaNum c || '\'' == c
