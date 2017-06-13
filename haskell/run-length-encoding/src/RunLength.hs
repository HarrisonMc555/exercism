module RunLength (decode, encode) where

import Data.Char

decode :: String -> String
decode "" = ""
decode encodedText = firstPortion ++ decode rest
  where (firstPortion, rest) = decodePortion encodedText

decodePortion :: String -> (String, String)
decodePortion "" = ("", "")
decodePortion encodedText@(c:_) = (replicate num letter, rest)
  where num = if isNumber c then read digits :: Int else 1
        (digits, letter:rest) = span isNumber encodedText

encode :: String -> String
encode "" = ""
encode text = firstPortion ++ encode rest
  where (firstPortion, rest) = encodePortion text

encodePortion :: String -> (String, String)
encodePortion "" = ("", "")
encodePortion s@(c:_) = (numStr ++ [c], remaining)
  where numStr = if numChars == 1 then "" else show numChars
        (firsts, remaining) = span (== c) s
        numChars = length firsts
