module Raindrops (convert) where

convert :: Int -> String
convert n
  | null rains = show n
  | otherwise  = rains
  where rains = getRains n

rainList :: [(Int, String)]
rainList = [ (3, "Pling")
           , (5, "Plang")
           , (7, "Plong")]

getRains :: Int -> String
getRains n = concatMap snd $ filter (divisibleBy n . fst) rainList

divisibleBy :: Integral a => a -> a -> Bool
divisibleBy num divisor = num `mod` divisor == 0
