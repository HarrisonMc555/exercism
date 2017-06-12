module ETL (transform) where

import Data.Map (Map, empty, insert, foldrWithKey)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform = foldrWithKey insertString empty

insertString :: k -> String -> Map Char k -> Map Char k
insertString k s m = foldr acc m s
  where acc c = insert (toLower c) k
