module Change (findFewestCoins) where

import Data.Maybe (mapMaybe, listToMaybe)
import Control.Monad (join)
import Data.List (genericIndex, sortOn, genericIndex, genericDrop
                 -- Only used in "cool but unused functions"
                 , minimumBy, maximumBy)
-- Only used in "cool but unused functions"
import Data.Ord (comparing)

type Solution = Maybe [Integer]

findFewestCoins :: Integer -> [Integer] -> Solution
findFewestCoins target coins
  | target < 0 = Nothing
  | otherwise  = dynamicProgram nextSolution' base target
  where nextSolution' = nextSolution coins
        base = Just []

nextSolution :: [Integer] -> [Solution] -> Solution
nextSolution coins prevSolutions =
  listToMaybe $ sortOn length $ mapMaybe coinSoln coins
  where coinSoln coin = (coin:) <$> join (maybeIndex prevSolutions (coin - 1))

maybeIndex :: Integral i => [a] -> i -> Maybe a
maybeIndex as i = listToMaybe $ genericDrop i as

-- Implement dynamic programming
dynamicProgram :: Integral i =>
  ([a] -> a) -> -- A dynamic function, one that uses previous results to
                -- construct the next result
  a ->          -- Base case
  i ->          -- The index of the final solution
  a             -- The final solution
dynamicProgram f base index = last $ scanIterate f base `genericIndex` index

scanIterate :: ([a] -> a) -> a -> [[a]]
scanIterate f base = map reverse $ iterate g [base]
  where g xs = f xs:xs

----------------------------------------
-- Unused but cool code

_minimumOn, _maximumOn :: Ord b => (a -> b) -> [a] -> a
_minimumOn f = minimumBy (comparing f)
_maximumOn f = maximumBy (comparing f)

_dynamicFoldl :: ([b] -> a -> b) -> b -> [a] -> b
_dynamicFoldl = go
  where go f b as = head $ _dynamicScanl f b as

_dynamicScanl :: ([b] -> a -> b) -> b -> [a] -> [b]
_dynamicScanl f b = go [b]
  -- where go :: [b] -> [a] -> [b]
  where go _ [] = []
        go bs (a:as) = let b' = f bs a
                       in b' : go (b':bs) as
