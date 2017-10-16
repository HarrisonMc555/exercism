module Change (findFewestCoins) where

import Data.List (sort, genericReplicate, genericIndex, minimumBy)
import Data.Maybe (catMaybes, isJust, fromJust)

type Solution = Maybe [Integer]

findFewestCoins :: Integer -> [Integer] -> Solution
findFewestCoins change coins
  | change < 0 = Nothing -- no negative change
  | null coins = Nothing
  | change < smallestCoinValue = Nothing
  | otherwise  = Just $ findChange change coins
  where smallestCoinValue = minimum coins


findChange :: Integer -> [Integer] -> [Integer]
findChange change coins =
  let coins' = sort coins
      useCoin coin (changeLeft, coinsSoFar) =
        let (numCoins, changeLeft') = changeLeft `quotRem` coin
            coinsToAdd = genericReplicate numCoins coin
        in (changeLeft', coinsToAdd ++ coinsSoFar)
      (changeLeftOver, changeCoins) = foldr useCoin (change, []) coins'
  in changeCoins

-- findFewestCoins' :: Integer -> [Integer] -> Solution
-- findFewestCoins' change coins =
--   let prevSolution = findFewestCoins' (change - 1) coins
--   in if change < 0
--      then Nothing
--      else dynamicFindFewestCoins

dynamicFindFewestCoins :: Integer -> [Integer] -> [Solution] -> Solution
dynamicFindFewestCoins change coins prevSolutions =
  if invalid || null coinSolutionPairs
  then Nothing
  else Just solution
  where invalid = null coins ||
                  change < minimum coins
        coinSolutionPairs = [ (coin, fromJust coinSolution)
                            | coin <- coins
                            , let index = change - coin
                            , index >= 0
                            , let coinSolution = genericIndex prevSolutions index
                            , isJust coinSolution
                            ]
        orderCoinSolutionPair (_, s1) (_, s2) = length s1 `compare` length s2
        bestCoinSolutionPair = minimumBy orderCoinSolutionPair coinSolutionPairs
        solution = case bestCoinSolutionPair
                   of (coin, solution) -> (coin:) solution

-- It would be super cool to have a function like scanl that took the entire
-- lsit of results so far as one of the inputs to the function instead of just
-- the current result so far. You could use that for the actual findFewestCoins
-- function.
