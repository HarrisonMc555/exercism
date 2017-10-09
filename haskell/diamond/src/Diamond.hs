module Diamond (diamond) where

diamond :: Char -> [String]
diamond c = let cs = ['A'..c]
                n = length cs
                topLines = zipWith (diamondLine (n-1)) [n-1,n-2..0] cs
                bottomLines = drop 1 $ reverse topLines
            in topLines ++ bottomLines


diamondLine :: Int -> Int -> Char -> String
diamondLine numTotalSpaces numLeftSpaces c = left ++ right
  where left           = leftSpaces ++ [c] ++ rightSpaces
        right          = drop 1 $ reverse left
        leftSpaces     = replicate numLeftSpaces ' '
        numRightSpaces = numTotalSpaces - numLeftSpaces
        rightSpaces    = replicate numRightSpaces ' '
