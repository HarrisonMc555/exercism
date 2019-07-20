module Beer (song) where

import Data.Char (toUpper)
import Data.List (intercalate)

song :: String
song = intercalate "\n" $ map verse [99,98..0]

verse :: Integer -> String
verse n = wallString n ++ "\n" ++ passAroundString n ++ "\n"

wallString :: Integer -> String
wallString numBottles = upcaseFirst bottleString' ++
                        " of beer on the wall, " ++
                        bottleString' ++ " of beer."
  where bottleString' = bottleString numBottles

passAroundString :: Integer -> String
passAroundString numBottles
  | numBottles > 0 = "Take " ++ one ++ " down and pass it around, " ++
                     nextBottlesString ++ " of beer on the wall."
  | otherwise      = storeString
  where one = if numBottles == 1 then "it" else "one"
        nextBottlesString = bottleString $ numBottles - 1

storeString :: String
storeString = "Go to the store and buy some more, 99 bottles of beer on the wall."

bottleString :: Integer -> String
bottleString numBottles
  | numBottles >  1 = numString ++ " bottles"
  | numBottles == 1 = numString ++ " bottle"
  | otherwise       = "no more bottles"
  where numString = show numBottles

upcaseFirst :: String -> String
upcaseFirst (c:cs) = toUpper c : cs
upcaseFirst ""     = ""