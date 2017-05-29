module ETL (transform) where

import Data.Map (Map, empty, insert, foldrWithKey)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform = foldrWithKey insertString empty

insertString :: k -> String -> Map Char k -> Map Char k
insertString _ ""     m = m
insertString x (c:cs) m = insertString x cs (insert c' x m)
  where c' = toLower c
