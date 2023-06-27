module IsbnVerifier (isbn) where

import Data.Maybe (fromMaybe)
import Data.Char (isDigit, digitToInt, toUpper)

isbn :: String -> Bool
isbn cs =
  let cs'     = filterNonNumbers cs
      leading = init cs'
      check   = last cs'
      isValid = do leadingNums <- traverse isbnLeadingCharToNum leading
                   checkNum    <- isbnCheckCharToNum check
                   return $ isValidIsbnNums leadingNums checkNum
  in
    length cs' == 10 && fromMaybe False isValid

isbnLeadingCharToNum :: Char -> Maybe Int
isbnLeadingCharToNum c
  | isDigit c = Just $ digitToInt c
  | otherwise = Nothing

isbnCheckCharToNum :: Char -> Maybe Int
isbnCheckCharToNum c
  | isDigit c = Just $ digitToInt c
  | c' == 'X' = Just 10
  | otherwise = Nothing
    where c' = toUpper c

isValidIsbnNums ::
  [Int] -> -- Leading Nums
  Int ->   -- Check num
  Bool
isValidIsbnNums leading check =
  let nums     = leading ++ [check]
      products = zipWith (*) nums [10, 9..]
      sum'     = sum products
      result   = sum' `mod` 11
  in result == 0

filterNonNumbers :: String -> String
filterNonNumbers = filter isValidIsbnDigit
  where isValidIsbnDigit c = isDigit c || c == 'X'