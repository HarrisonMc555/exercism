module Brackets (arePaired) where

import Control.Monad (foldM)

arePaired :: String -> Bool
arePaired = isDone . foldM processChar ""
  where isDone (Just bs) = null bs
        isDone Nothing = False

type BracketStack = String
processChar :: BracketStack -> Char -> Maybe BracketStack
processChar bs c
  | isClosingBracket c = if matchesBracketStack
                         then Just (tail bs)
                         else Nothing
  | otherwise = case lookup c brackets of
                  Just b -> Just (b:bs)
                  Nothing -> Just bs
  where isClosingBracket = (`elem` closingBrackets)
        closingBrackets = map snd brackets
        matchesBracketStack = not (null bs) && c == head bs


brackets :: [(Char, Char)]
brackets = [ ('(', ')')
           , ('[', ']')
           , ('{', '}')
           ]
