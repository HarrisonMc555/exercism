module RailFenceCipher (encode, decode) where

import Data.Vector (Vector, fromList)

data Direction = Down | Up

encode :: Int -> String -> String
encode x s = s

decode :: Int -> String -> String
decode = error "You need to implement this function!"

toRails :: Int -> String -> Vector (Vector Char)
toRails _ _ = fromList []
-- toRails num = go 0 Down []
--   where go _ _ acc "" = acc
--         go i dir (c:cs) =
--           case (i, dir) of
--             (num - 1, Down) -> go (i - 1) Up []
