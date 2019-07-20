module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

encode :: String -> String
encode xs = unwords . transpose . groupsOf nc $ xs'
  where xs' = normalize xs
        nc = numColumns . length $ xs'

normalize :: String -> String
normalize = map toLower . filter isAlphaNum

numColumns :: Int -> Int
numColumns = ceiling . sqrt' . fromIntegral
  where sqrt' = sqrt :: Double -> Double -- For `stack test --pedantic`

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs
  | n < 0 = error "invalid size"
  | n == 0 = []
  | otherwise = groupsOf' xs
  where groupsOf' [] = []
        groupsOf' xs' = group : groupsOf' rest
          where (group, rest) = splitAt n xs'