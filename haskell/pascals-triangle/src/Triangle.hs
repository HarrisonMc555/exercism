module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x = take x $ iterate pascal [1]

pascal :: [Integer] -> [Integer]
pascal row = 1 : pascal' row
  where pascal' (x:xs@(x':_)) = x + x' : pascal' xs
        pascal' _ = [1]
