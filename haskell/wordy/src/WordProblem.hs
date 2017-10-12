module WordProblem (answer) where

import Data.Maybe (fromJust)
import Control.Monad (guard, liftM, forM)
import Data.Char (toUpper, isLetter)
import Data.List (groupBy)

data Op = Plus | Minus | MultipliedBy | DividedBy deriving (Show, Read)

-- This is getting to complicated, need to find a way to break it up. Also I'm
-- pretty sure I could use `groupOn` here.
answer :: String -> Maybe Integer
answer problem =
  let ws = words' problem
  in do
    ws' <- ws
    guard $ isValidPrefix ws'
    (pre1:pre2:firstS:rest) <- ws
    first <- stringToInteger' firstS
    numOpPairs <- toNumberOpPairs' rest
    let applyNumOpPair i1 (o, i2) = let f = opToFunction o
                                    in i1 `f` i2
    return $ foldl applyNumOpPair first numOpPairs

-- Using read here is sort of janky. I should probably just intercalate " " and
-- check against raw strings.
stringsToOp :: [String] -> Op
stringsToOp = read . filter isLetter . concatMap toTitleCase

stringToOp :: String -> Op
stringToOp = stringsToOp . words

stringToOp' :: String -> Maybe Op
stringToOp' s = case s of
                  "plus"          -> Just Plus
                  "minus"         -> Just Minus
                  "multiplied by" -> Just MultipliedBy
                  "divided by"    -> Just DividedBy
                  _               -> Nothing

opToFunction :: (Integral a) => Op -> a -> a -> a
opToFunction op = case op of
  Plus         -> (+)
  Minus        -> (-)
  MultipliedBy -> (*)
  DividedBy    -> div

stringToFunction :: Integral a => String -> a -> a -> a
stringToFunction = opToFunction . stringToOp

words' [] = error "Empty string"
words' cs = let (rest, postfix) = splitLast cs
            in toMaybe (words rest) (isValidPostfix postfix)

additionS :: String
additionS = "plus"

prefixS :: [String]
prefixS = ["What", "is"]

isValidPrefix :: [String] -> Bool
isValidPrefix = (prefixS ==) . take 2

postfixC :: Char
postfixC = '?'

isValidPostfix :: Char -> Bool
isValidPostfix = (postfixC ==)

splitLast :: [a] -> ([a], a)
splitLast []     = error "Empty list"
splitLast [a]    = ([], a)
splitLast (a:as) = let (as', a') = splitLast as
                   in (a:as', a')

toMaybe :: a -> Bool -> Maybe a
toMaybe a b = if b then Just a else Nothing

toTitleCase :: String -> String
toTitleCase (c:cs) = (toUpper c : cs)
toTitleCase "" = ""

-- https://stackoverflow.com/a/30029229/7343786
isInteger :: String -> Bool
isInteger s = case (reads s) :: [(Integer, String)] of
                [(_, "")] -> True
                _         -> False

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = let f' x y = f x == f y
            in groupBy f'

-- List of words that are pairings of operations and numbers. Expects the first
-- to be an operation
toNumberOpPairs :: [String] -> [(Op, Integer)]
toNumberOpPairs = let parts = map unwords . groupOn isInteger
                      partPairs = pairs . parts
                      readPair (oS, iS) = (stringToOp oS, read iS)
                  in map readPair . partPairs

toNumberOpPairs' :: [String] -> Maybe [(Op, Integer)]
toNumberOpPairs' = go
  where go s = let parts = map unwords . groupOn isInteger
               in do partPairStrings <- pairs' . parts $ s
                     let readPair (oS, iS) = do
                           op <- stringToOp' oS
                           i  <- stringToInteger' iS
                           return (op, i)
                     partPairs <- forM partPairStrings readPair
                     return partPairs
-- toNumberOpPairs' :: [String] -> Maybe [(Op, Integer)]
-- toNumberOpPairs' = let parts = map unwords . groupOn isInteger
--                        maybePartPairs = pairs . parts
--                        readPair (oS, iS) = do
--                          op <- stringToOp' oS
--                          i  <- stringToInteger' iS
--                          return (op, i)
--                      in sequence . map readPair . pairs . parts
                           
stringToInteger' :: String -> Maybe Integer
stringToInteger' s = case (reads s) :: [(Integer, String)] of
                       [(x, "")] -> Just x
                       _         -> Nothing

pairs :: [a] -> [(a, a)]
pairs (x:x':xs) = (x, x') : (pairs xs)
pairs _ = []

pairs' :: [a] -> Maybe [(a, a)]
pairs' (x:x':xs) = ((x, x') :) <$> pairs' xs
pairs' [x] = Nothing
pairs' [] = Just []
