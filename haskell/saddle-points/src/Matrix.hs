module Matrix (saddlePoints) where

import Data.Array (Array, (!), bounds)
import Data.Ix (Ix)
import Data.List (intersect, maximumBy, minimumBy, concatMap)

saddlePoints :: (Enum i, Ix i, Ord e) => Array (i, i) e -> [(i, i)]
saddlePoints matrix = map fst $ intersect rowMaxs colMins
  where rowMaxs = concatMap assocMaxs . rows $ matrix
        colMins = concatMap assocMins . cols $ matrix

assocMaxs :: (Ord e, Eq e) => [(i, e)] -> [(i, e)]
assocMaxs ies = filter (elemEq elemMax) ies
  where elemEq (_, e1) (_, e2) = e1 == e2
        elemMax = maximumBy compareElem ies
        compareElem (_, e1) (_, e2) = compare e1 e2

assocMins :: (Ord e, Eq e) => [(i, e)] -> [(i, e)]
assocMins ies = filter (elemEq elemMin) ies
  where elemEq (_, e1) (_, e2) = e1 == e2
        elemMin = minimumBy compareElem ies
        compareElem (_, e1) (_, e2) = compare e1 e2

rows :: (Enum i, Ix i) => Array (i, i) e -> [[((i, i), e)]]
rows a = [row a nRow | nRow <- [minRow.. maxRow]]
  where (minRow, maxRow) = rowBounds a

cols :: (Enum i, Ix i) => Array (i, i) e -> [[((i, i), e)]]
cols a = [col a nCol | nCol <- [minCol.. maxCol]]
  where (minCol, maxCol) = colBounds a

row :: (Enum i, Ix i) => Array (i, i) e -> i -> [((i, i), e)]
row a nRow = [(i, a ! i) | nCol <- [minCol.. maxCol], let i = (nRow, nCol)]
  where (minCol, maxCol) = colBounds a

col :: (Enum i, Ix i) => Array (i, i) e -> i -> [((i, i), e)]
col a nCol = [(i, a ! i) | nRow <- [minRow.. maxRow], let i = (nRow, nCol)]
  where (minRow, maxRow) = rowBounds a

rowBounds :: Ix i => Array (i, i) e -> (i, i)
rowBounds a = (minRow, maxRow)
  where ((minRow, _), (maxRow, _)) = bounds a

colBounds :: Ix i => Array (i, i) e -> (i, i)
colBounds a = (minCol, maxCol)
  where ((_, minCol), (_, maxCol)) = bounds a