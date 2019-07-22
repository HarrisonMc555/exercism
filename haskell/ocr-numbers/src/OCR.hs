module OCR (convert) where

import Control.Arrow ((>>>))
import Data.Char (intToDigit)
import Data.Maybe (fromMaybe)
import Data.List (transpose, intercalate)

convert :: String -> String
convert = splitLines >>>
          map convertLine >>>
          intercalate ","

convertLine :: [String] -> String
convertLine = splitDigits >>> map convertDigit

convertDigit :: [String] -> Char
convertDigit = flip lookup ocrDigits >>>
               fmap intToDigit >>>
               fromMaybe '?'

splitLines :: String -> [[String]]
splitLines = lines >>>
             chunksOf 4

splitDigits :: [String] -> [[String]]
splitDigits = map (chunksOf 3) >>>
              transpose

-- http://hackage.haskell.org/package/text-1.2.2.2/docs/src/Data-Text.html#chunksOf
chunksOf :: Int -> [a] -> [[a]]
chunksOf k = go
  where go xs = case splitAt k xs of
                  (a, b) | null a    -> []
                         | otherwise -> a : go b

ocrDigits :: [([String], Int)]
ocrDigits = [ (ocrZero  , 0)
            , (ocrOne   , 1)
            , (ocrTwo   , 2)
            , (ocrThree , 3)
            , (ocrFour  , 4)
            , (ocrFive  , 5)
            , (ocrSix   , 6)
            , (ocrSeven , 7)
            , (ocrEight , 8)
            , (ocrNine  , 9)
            ]

ocrZero, ocrOne, ocrTwo, ocrThree, ocrFour :: [String]
ocrFive, ocrSix, ocrSeven, ocrEight, ocrNine :: [String]
ocrZero  = [ " _ "
           , "| |"
           , "|_|"
           , "   " ]
ocrOne   = [ "   "
           , "  |"
           , "  |"
           , "   " ]
ocrTwo   = [ " _ "
           , " _|"
           , "|_ "
           , "   " ]
ocrThree = [ " _ "
           , " _|"
           , " _|"
           , "   " ]
ocrFour  = [ "   "
           , "|_|"
           , "  |"
           , "   " ]
ocrFive  = [ " _ "
           , "|_ "
           , " _|"
           , "   " ]
ocrSix   = [ " _ "
           , "|_ "
           , "|_|"
           , "   " ]
ocrSeven = [ " _ "
           , "  |"
           , "  |"
           , "   " ]
ocrEight = [ " _ "
           , "|_|"
           , "|_|"
           , "   " ]
ocrNine  = [ " _ "
           , "|_|"
           , " _|"
           , "   " ]
