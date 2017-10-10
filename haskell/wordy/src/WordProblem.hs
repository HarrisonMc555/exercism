module WordProblem (answer) where

import Data.Maybe (fromJust)
import Control.Monad (guard)
import Data.Char (toUpper)
import Data.List (groupBy)

data Op = Plus | Minus | MultipliedBy | DividedBy deriving (Show, Read)

-- This is getting to complicated, need to find a way to break it up. Also I'm
-- pretty sure I could use `groupOn` here.
answer :: String -> Maybe Integer
answer problem =
  let ws = words' problem
      ws' = fromJust ws
      [pre1, pre2, left, op, right] = ws'
  in do
    ws' <- ws
    guard $ isValidPrefix ws'
    let (pre1:pre2:firstS:rest) = ws'
        (opSs, rightS) = splitLast rest
        first = read firstS :: Integer
        opO   = stringToOp $ concatMap toTitleCase opSs
        right = read rightS :: Integer
        op    = opToFunction opO
    return $ first `op` right

-- Using read here is sort of janky. I should probably just intercalate " " and
-- check against raw strings.
stringToOp :: String -> Op
stringToOp s = let s' = toTitleCase s
         in read s'

opToFunction :: (Integral a) => Op -> a -> a -> a
opToFunction op = case op of
  Plus         -> (+)
  Minus        -> (-)
  MultipliedBy -> (*)
  DividedBy    -> div

stringToFunction :: Integral a => String -> a -> a -> a
stringToFunction = opToFunction . stringToOp

-- applyWordyFunction :: Integral a => a -> String -> a
-- applyWordyFunction 

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
toTitleCase "" = ""
toTitleCase (c:cs) = (toUpper c : cs)

-- https://stackoverflow.com/a/30029229/7343786
isInteger :: String -> Bool
isInteger s = case (reads s) :: [(Integer, String)] of
                [(_, "")] -> True
                _         -> False

groupOn :: (a -> Bool) -> [a] -> [[a]]
groupOn f = let f' x y = f x == f y
            in groupBy f'
