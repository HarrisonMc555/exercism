module Brackets (arePaired) where

import Data.Maybe (fromJust)
import Control.Monad (foldM)

arePaired :: String -> Bool
arePaired = finalCheck . foldM processChar ""
  where finalCheck (Just bs) = null bs
        finalCheck Nothing = False

type BracketStack = String
processChar :: BracketStack -> Char -> Maybe BracketStack
processChar bs c
  | isOpen c = let b = fromJust $ lookup c brackets
               in Just (b:bs)
  | isClose c = if isMatch then Just (tail bs) else Nothing
  | otherwise = Just bs
  where isMatch = (not . null) bs && c == head bs


brackets :: [(Char, Char)]
brackets = [ ('(', ')')
           , ('[', ']')
           , ('{', '}')
           ]

isOpen, isClose :: Char -> Bool
isOpen = (`elem` openBrackets)
  where openBrackets = map fst brackets
isClose = (`elem` closeBrackets)
  where closeBrackets = map snd brackets
