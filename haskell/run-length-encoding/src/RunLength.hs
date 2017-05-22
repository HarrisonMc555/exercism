module RunLength (decode, encode) where

import Data.Char
import Text.Read

decode :: String -> String
decode "" = ""
decode encodedText = firstPortion ++ decode rest
  where firstPortion = fst decodingTuple
        rest = snd decodingTuple
        decodingTuple = decodePortion encodedText

decodePortion "" = ("", "")
decodePortion encodedText = (replicate num char, rest)
  where num = if isNumber (head encodedText)
              then read (takeWhile isNumber encodedText) :: Int
              else 1
        char = head afterNum
        rest = tail afterNum
        afterNum = dropWhile isNumber encodedText

encode :: String -> String
encode "" = ""
encode text = firstPortion ++ encode rest
  where firstPortion = fst encodingTuple
        rest = snd encodingTuple
        encodingTuple = encodePortion text

encodePortion "" = ("", "")
encodePortion text = (numStr ++ [firstChar], remainingText)
  where firstChar = head text
        numStr = if numChars == 1 then "" else show numChars
        numChars = length $ takeWhile isFirstChar text
        remainingText = dropWhile isFirstChar text
        isFirstChar c = c == firstChar
