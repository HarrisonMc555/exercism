module Scrabble (scoreLetter, scoreWord) where

import Data.Map (Map, empty, insert, findWithDefault)
import Data.Char (toUpper)

scoreLetters :: [(Integer, String)]
scoreLetters =
  [ (1,  ['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T'])
  , (2,  ['D', 'G'])
  , (3,  ['B', 'C', 'M', 'P'])
  , (4,  ['F', 'H', 'V', 'W', 'Y'])
  , (5,  ['K'])
  , (8,  ['J', 'X'])
  , (10, ['Q', 'Z'])
  ]

letterScores :: Map Char Integer
letterScores = foldr (uncurry insertString) empty scoreLetters

scoreLetter :: Char -> Integer
scoreLetter letter = findWithDefault 0 (toUpper letter) letterScores

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter

insertString :: k -> String -> Map Char k -> Map Char k
insertString _ "" m     = m
insertString x (c:cs) m = insertString x cs (insert c' x m)
  where c' = toUpper c