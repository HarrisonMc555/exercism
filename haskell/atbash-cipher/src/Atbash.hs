module Atbash (decode, encode) where

import Data.Maybe (mapMaybe)
import Data.Char (toLower, isDigit)

cipher :: [(Char, Char)]
cipher = zip ['a'..'z'] ['z','y'..'a']

transformChar :: Char -> Maybe Char
transformChar c =
  if isDigit c
  then Just c
  else lookup (toLower c) cipher

transform :: String -> String
transform = mapMaybe transformChar

decode :: String -> String
decode = transform

encode :: String -> String
encode = unwords . groupsOf 5 . transform

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs
  | n <= 0 = error "invalid size"
  | otherwise = groupsOf' xs
  where groupsOf' [] = []
        groupsOf' xs' = group : groupsOf' rest
          where (group, rest) = splitAt n xs'
