module Series (slices) where

import Control.Arrow ((>>>))
import Data.Maybe (mapMaybe)

slices :: Int -> String -> [[Int]]
slices n = intList >>> windows n

intList :: String -> [Int]
intList = map (read . pure)

windows :: Int -> [a] -> [[a]]
windows 0 [] = [[]]
windows _ [] = []
windows size list@(_:rest) =
  case takeIfComplete size list of
    Just window -> window : windows size rest
    Nothing -> []

takeIfComplete :: Int -> [a] -> Maybe [a]
takeIfComplete 0 _ = Just []
takeIfComplete _ [] = Nothing
takeIfComplete n (x:xs) = (x:) <$> takeIfComplete (n - 1) xs
