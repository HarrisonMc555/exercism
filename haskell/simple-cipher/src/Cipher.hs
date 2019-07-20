module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr, toLower)
import System.Random (getStdGen, randomRs, randomRIO)

data Code = Encode | Decode

caesarDecode, caesarEncode :: String -> String -> String
caesarDecode = caesarChange Decode
caesarEncode = caesarChange Encode

caesarChange :: Code -> String -> String -> String
caesarChange e key = let changeChar = case e of Encode -> caesarEncodeChar
                                                Decode -> caesarDecodeChar in
                       zipWith changeChar (cycle key)

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  g <- getStdGen
  let key = take 100 (randomRs ('a', 'z') g)
      encodedText = caesarEncode key text
  _ <- randomRIO ('a', 'a') -- to make the next run different
  return (key, encodedText)

caesarDecodeChar, caesarEncodeChar :: Char -> Char -> Char
caesarDecodeChar = caesarChangeChar Dec
caesarEncodeChar = caesarChangeChar Inc

caesarChangeChar :: IncDec -> Char -> Char -> Char
caesarChangeChar i key = let incDecChar =
                               case i of Inc -> caesarIncrementChar
                                         Dec -> caesarDecrementChar in
                           incDecChar (caesarOrd key)

caesarIncrementChar, caesarDecrementChar :: Int -> Char -> Char
caesarIncrementChar = caesarIncDecChar Inc
caesarDecrementChar = caesarIncDecChar Dec

data IncDec = Inc | Dec

caesarIncDecChar :: IncDec -> Int -> Char -> Char
caesarIncDecChar b i c = let incDec = case b of Inc -> (+)
                                                Dec -> (-) in
  caesarChr $ (caesarOrd c `incDec` i) `mod` numLetters

caesarOrd :: Char -> Int
caesarOrd = subtract (ord 'a') . ord . toLower

caesarChr :: Int -> Char
caesarChr = chr . (ord 'a' +)

numLetters :: Int
numLetters = length ['a'..'z']