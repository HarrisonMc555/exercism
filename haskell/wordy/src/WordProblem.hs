module WordProblem (answer) where

import Control.Monad (guard, forM)
import Data.List (groupBy)

data Op = Plus | Minus | MultipliedBy | DividedBy deriving (Show, Read)

answer :: String -> Maybe Integer
answer problem = do
  (problem', lastChar) <- mSplitLast problem
  guard $ isValidPostfix lastChar
  let ws = words problem'
  guard $ isValidPrefix ws
  (firstS, rest) <- case ws of
    (_:_:firstS:rest) -> Just (firstS, rest)
    _ -> Nothing
  first <- stringToMInteger firstS
  numOpPairs <- toNumberMOpPairs rest
  return $ foldl applyNumOpPair first numOpPairs

stringToMOp :: String -> Maybe Op
stringToMOp s = case s of
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

stringToMInteger :: String -> Maybe Integer
stringToMInteger s = case reads s :: [(Integer, String)] of
                       [(x, "")] -> Just x
                       _         -> Nothing

toNumberMOpPairs :: [String] -> Maybe [(Op, Integer)]
toNumberMOpPairs = go
  where go s = let parts = map unwords . groupOn isInteger
               in do partPairStrings <- mPairs . parts $ s
                     let readPair (oS, iS) = do
                           op <- stringToMOp oS
                           i  <- stringToMInteger iS
                           return (op, i)
                     forM partPairStrings readPair

applyNumOpPair :: Integer -> (Op, Integer) -> Integer
applyNumOpPair i1 (o, i2) = let f = opToFunction o
                            in i1 `f` i2
                       
postfixC :: Char
postfixC = '?'

isValidPrefix :: [String] -> Bool
isValidPrefix ("What":"is":_) = True
isValidPrefix _ = False
-- isValidPrefix = go
--   where go xs = xs `longerThan` 3 &&
--                 prefixS == take 2 xs
--         longerThan xs n = not . null $ drop n xs

isValidPostfix :: Char -> Bool
isValidPostfix = (postfixC ==)

-- https://stackoverflow.com/a/30029229/7343786
isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
                [(_, "")] -> True
                _         -> False

splitLast :: [a] -> ([a], a)
splitLast []     = error "Empty list"
splitLast [a]    = ([], a)
splitLast (a:as) = let (as', a') = splitLast as
                   in (a:as', a')

mSplitLast :: [a] -> Maybe ([a], a)
mSplitLast [] = Nothing
mSplitLast xs = Just $ splitLast xs

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = let f' x y = f x == f y
            in groupBy f'

mPairs :: [a] -> Maybe [(a, a)]
mPairs (x:x':xs) = ((x, x') :) <$> mPairs xs
mPairs [_] = Nothing
mPairs [] = Just []