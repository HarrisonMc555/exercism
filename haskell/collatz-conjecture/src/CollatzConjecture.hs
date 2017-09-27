module CollatzConjecture (collatz) where

import Data.List (find)

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise =
      let pairs = collatzList n
          done  = (== 1) . snd
          end   = find done pairs
      in  fst <$> end

nextCollatz :: Integer -> Integer
nextCollatz n
 | n <= 0    = error "Invalid collatz number"
 | even n    = n `div` 2
 | otherwise = 3*n + 1

collatzList :: Integer -> [(Integer, Integer)]
collatzList = iterateAccumLength nextCollatz

iterateAccumLength :: Integral n => (a -> a) -> a -> [(n, a)]
iterateAccumLength = go
  where go f = zip [0..] . iterate f

