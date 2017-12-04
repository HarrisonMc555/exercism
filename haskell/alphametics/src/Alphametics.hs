module Alphametics (solve) where

import qualified Calculator as Calc
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (sort, nub)
import Data.Map (Map)
import qualified Data.Map as Map

data Associativity = LeftAssoc | NonAssoc | RightAssoc

solve :: String -> Maybe [(Char, Int)]
solve puzzle = error "You need to implement this function."

isSolution :: String -> Map Char Int -> Bool
isSolution s m = let ws = words s
                     answer = read . last $ ws
                     equation = concat . init . init $ ws
                     evaluated = eval m equation
                     equalSignInRightSpot = (last . init $ ws) == "=="
                     correct = maybe False (== answer) evaluated
                 in equalSignInRightSpot && correct

eval :: Map Char Int -> String -> Maybe Int
eval m s = let (ws, opsS) = everyOther $ words s
               nums = map (replace m) ws
               numsS = map show nums
               replacedS = concat $ intercalateLists numsS opsS
               evaluated = Calc.eval replacedS
           in case evaluated of
                Right x -> Just x
                Left _  -> Nothing

replace :: Map Char Int -> String -> Int
replace m = digitsToInt . catMaybes . map (flip Map.lookup m)

digitsToInt :: [Int] -> Int
digitsToInt = read . concat . map show

uniq :: Ord a => [a] -> [a]
uniq = nub . sort

digits :: [Int]
digits = [0..9]

everyOther :: [a] -> ([a], [a])
everyOther xs = let (left, right, _) = foldl f ([], [], True) xs
                in (reverse left, reverse right)
  where f (left, right, b) x = if b then (x:left, right, not b)
                               else      (left, x:right, not b)

intercalateLists :: [a] -> [a] -> [a]
intercalateLists = intercalateLists' True
  where intercalateLists' _ xs [] = xs
        intercalateLists' _ [] ys = ys
        intercalateLists' first xs'@(x:xs) ys'@(y:ys) =
          if first
          then x : intercalateLists' (not first) xs ys'
          else y : intercalateLists' (not first) xs' ys
