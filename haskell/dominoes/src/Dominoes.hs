module Dominoes (chain) where

import Control.Monad (MonadPlus, mzero)
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Safe (lastMay)
import Data.List.Extra (firstJust)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

type Domino = (Int, Int)
type Solution = [Domino]
data PartialSolution = PartialSolution Domino [Domino]

chain :: [Domino] -> Maybe [Domino]
chain dominoes =
  case dominoes of
    d : rest -> let restSet = MultiSet.fromList rest
                    partialSolution = PartialSolution d []
                in findSolution partialSolution restSet
    [] -> Just []

findSolution :: PartialSolution -> MultiSet Domino -> Maybe Solution
findSolution ps@(PartialSolution domino _) remaining
  | null remaining = ps & fromBool isSolution & fmap toSolution
  | otherwise =
    let findSolutionHelper :: Domino -> Maybe Solution
        findSolutionHelper next =
          do oriented <- orientedMatch domino next
             let newRemaining = MultiSet.delete next remaining
                 newPartialSolution = addToSolution oriented ps
             newRemaining & findSolution newPartialSolution
    in firstJust findSolutionHelper (MultiSet.distinctElems remaining)

orientedMatch :: Domino -> Domino -> Maybe Domino
orientedMatch (pipToMatch, _) (pip1, pip2)
  | pip1 == pipToMatch = Just (pip2, pip1)
  | pip2 == pipToMatch = Just (pip1, pip2)
  | otherwise = Nothing

isSolution :: PartialSolution -> Bool
isSolution (PartialSolution domino rest) =
  let firstPips = fst domino
      lastPips  = rest & lastMay & fmap snd & fromMaybe (snd domino)
  in firstPips == lastPips

addToSolution :: Domino -> PartialSolution -> PartialSolution
addToSolution d (PartialSolution oldHead rest) =
  PartialSolution d (oldHead : rest)

toSolution :: PartialSolution -> Solution
toSolution (PartialSolution domino rest) =
  domino : rest

fromBool :: MonadPlus f => (a -> Bool) -> a -> f a
fromBool f a =
  if f a then return a else mzero
