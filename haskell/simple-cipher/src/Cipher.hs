module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr, toLower)
import System.Random (getStdGen, randomRs, randomRIO)

caesarDecode :: String -> String -> String
caesarDecode key = zipWith caesarDecodeChar (cycle key)

caesarEncode :: String -> String -> String
caesarEncode key = zipWith caesarEncodeChar (cycle key)

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  g <- getStdGen
  let key = take 100 (randomRs ('a', 'z') g)
      encodedText = caesarEncode key text
  _ <- randomRIO ('a', 'z') -- to make the next run different
  return (key, encodedText)

caesarDecodeChar :: Char -> Char -> Char
caesarDecodeChar key char = caesarDecrementChar (caesarOrd key) char

caesarEncodeChar :: Char -> Char -> Char
caesarEncodeChar key char = caesarIncrementChar (caesarOrd key) char

caesarOrd :: Char -> Int
caesarOrd = subtract (ord 'a') . ord . toLower

caesarChr :: Int -> Char
caesarChr = chr . (ord 'a' +)

caesarIncrementChar :: Int -> Char -> Char
caesarIncrementChar i c = caesarChr $ (caesarOrd c + i) `mod` numLetters

caesarDecrementChar :: Int -> Char -> Char
caesarDecrementChar i c = caesarChr $ (caesarOrd c - i) `mod` numLetters

numLetters :: Int
numLetters = length ['a'..'z']

