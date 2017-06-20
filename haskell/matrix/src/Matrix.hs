module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Prelude hiding
  ( length
  , map
  , null
  , head
  , foldr
  , (++)
  , all
  )

import qualified Data.List as List

import Data.Vector hiding
  (fromList)

import qualified Data.Vector as Vector

data Matrix a = Matrix
                Int                 -- Number of rows
                Int                 -- Number of columns
                (Vector (Vector a)) -- Elements
  deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix _ c _) = c

column :: Int -> Matrix a -> Vector a
column c (Matrix _ _ v) = map (! c) v

flatten :: Matrix a -> Vector a
flatten (Matrix _ _ v) = foldr (++) empty v

fromList :: [[a]] -> Matrix a
fromList xss
  | List.null xss = Matrix 0 0 empty
  | all (ncols ==) (map length m) = Matrix nrows ncols m
  | otherwise = error "not same length rows"
  where m = Vector.fromList . List.map Vector.fromList $ xss
        nrows = length m
        ncols = length (head m) -- safe because List.null xss

fromString :: Read a => String -> Matrix a
fromString = fromList . List.map (List.map read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (nr, nc) m
  | nr*nc /= nr'*nc' = error "matrices have different numbers of elements"
  | otherwise = Matrix nr nc $ generate nr getRow
  where v = flatten m
        (nr', nc') = (rows m, cols m)
        getRow r = generate nc getElem
          where getElem c = v ! (nc*r + c)

row :: Int -> Matrix a -> Vector a
row r (Matrix _ _ v) = v ! r

rows :: Matrix a -> Int
rows (Matrix r _ _) = r

shape :: Matrix a -> (Int, Int)
shape (Matrix r c _) = (r, c)

transpose :: Matrix a -> Matrix a
transpose m@(Matrix nr nc _) =
  Matrix nc nr (Vector.fromList $ List.map getCol [0..nc-1])
  where getCol ind = column ind m
