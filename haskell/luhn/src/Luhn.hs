module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)
import Control.Arrow ((>>>))

isValid :: String -> Bool
isValid s = let s' = strip s
            in checkForm s' &&
               (readDigits >>> luhnDoubleEvens >>> sum >>>
                (`mod` 10) >>> (== 0) $ s')

luhnDouble :: Int -> Int
luhnDouble x = let x' = 2*x
               in if x' > 9
                  then x' - 9
                  else x'

luhnDoubleEvens :: [Int] -> [Int]
luhnDoubleEvens = reverse . doubleEvens . reverse
  where doubleEvens = zipWith (curry doubleIfEven) indices
        doubleIfEven (i, x) = if i `mod` 2 == 0
                              then luhnDouble x
                              else x
        indices = [1..] :: [Int]

strip :: String -> String
strip = filter (/= ' ')

readDigits :: String -> [Int]
readDigits = map digitToInt

checkForm :: String -> Bool
checkForm s = all isDigit s && length s > 1