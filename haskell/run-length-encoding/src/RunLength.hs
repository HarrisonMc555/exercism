module RunLength (decode, encode) where

import Data.Char

decode :: String -> String
decode "" = ""
decode encodedText = firstPortion ++ decode rest
  where (firstPortion, rest) = decodePortion encodedText

decodePortion :: String -> (String, String)
decodePortion "" = ("", "")
decodePortion encodedText = (replicate num char, rest)
  where num = if isNumber (head encodedText)
              then read (takeWhile isNumber encodedText) :: Int
              else 1
        (char:rest) = dropWhile isNumber encodedText

encode :: String -> String
encode "" = ""
encode text = firstPortion ++ encode rest
  where (firstPortion, rest) = encodePortion text

encodePortion :: String -> (String, String)
encodePortion "" = ("", "")
encodePortion text = (numStr ++ [firstChar], remainingText)
  where firstChar = head text
        numStr = if numChars == 1 then "" else show numChars
        numChars = length $ takeWhile isFirstChar text
        remainingText = dropWhile isFirstChar text
        isFirstChar c = c == firstChar
