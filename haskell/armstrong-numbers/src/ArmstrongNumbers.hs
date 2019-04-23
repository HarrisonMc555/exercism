module ArmstrongNumbers (armstrong) where

import Control.Arrow ((>>>))
import Data.Char (digitToInt)

armstrong :: (Show a, Integral a) => a -> Bool
armstrong num =
  let digits = getDigits num
      numDigits = length digits
      squaredDigits = map (^ numDigits) digits
  in sum squaredDigits == fromIntegral num

getDigits :: (Show a, Integral a) => a -> [Int]
getDigits = show >>> map digitToInt
