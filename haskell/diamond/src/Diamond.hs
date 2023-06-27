module Diamond (diamond) where

import Data.Char (isAlpha)

diamond :: Char -> Maybe [String]
diamond c = let cs          = ['A'..c]
                totalSpaces = length cs - 1
                leftSpaces  = [totalSpaces,totalSpaces-1..]
                topLines    = zipWith (diamondLine totalSpaces) leftSpaces cs
                bottomLines = drop 1 $ reverse topLines
            in
              if isAlpha c 
              then Just $ topLines ++ bottomLines 
              else Nothing

diamondLine :: Int -> Int -> Char -> String
diamondLine numTotalSpaces numLeftSpaces c = left ++ right
  where left           = leftSpaces ++ [c] ++ rightSpaces
        right          = drop 1 $ reverse left
        leftSpaces     = replicate numLeftSpaces ' '
        numRightSpaces = numTotalSpaces - numLeftSpaces
        rightSpaces    = replicate numRightSpaces ' '