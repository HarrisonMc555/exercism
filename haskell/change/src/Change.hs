module Change (findFewestCoins) where

import Data.List (genericIndex, sortOn, genericIndex, genericDrop)
import Data.Maybe (mapMaybe, listToMaybe)
import Control.Monad (join)

type Solution = Maybe [Integer]

findFewestCoins :: Integer -> [Integer] -> Solution
findFewestCoins target coins
  | target < 0 = Nothing
  | otherwise  = last solutionsThroughTarget
  where solutionsThroughTarget =
          scanIterate nextSolution' base `genericIndex` target
        nextSolution' = nextSolution coins
        base = Just []

nextSolution :: [Integer] -> [Solution] -> Solution
nextSolution coins prevSolutions =
  listToMaybe $ sortOn length $ mapMaybe coinSoln coins
  where coinSoln coin = (coin:) <$> join (maybeIndex prevSolutions (coin - 1))

maybeIndex :: Integral i => [a] -> i -> Maybe a
maybeIndex as i = listToMaybe $ genericDrop i as

scanIterate :: ([a] -> a) -> a -> [[a]]
scanIterate f base = map reverse $ iterate g [base]
  where g xs = f xs:xs


----------------------------------------
-- Unused but cool code

_dynamicFoldl :: ([b] -> a -> b) -> b -> [a] -> b
_dynamicFoldl = go
  where go f b as = head $ _dynamicScanl f b as

_dynamicScanl :: ([b] -> a -> b) -> b -> [a] -> [b]
_dynamicScanl f b = go [b]
  -- where go :: [b] -> [a] -> [b]
  where go _ [] = []
        go bs (a:as) = let b' = f bs a
                       in b' : go (b':bs) as
