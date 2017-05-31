module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs
  | any isDigit excess      = Nothing
  | isValidPhoneNumber nums = Just nums
  | otherwise               = Nothing
  where (digits, excess) = splitAt 11 $ filter isDigit xs
        nums = removeLeadingOne digits

removeLeadingOne :: String -> String
removeLeadingOne digits
  | length digits > 1 && head digits == '1' = tail digits
  | otherwise                               = digits

isValidPhoneNumber :: String -> Bool
isValidPhoneNumber digits = rightLength && areaNotOne && exchangeNotOne
  where rightLength    = length digits == 10
        areaNotOne     = isValidLeadingDigit $ head digits
        exchangeNotOne = isValidLeadingDigit $ digits !! 3

isValidLeadingDigit :: Char -> Bool
isValidLeadingDigit c = '2' <= c && c <= '9'
