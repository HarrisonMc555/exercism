{-# LANGUAGE NumericUnderscores #-}
module ResistorColors (Color(..), Resistor(..), label, ohms) where

import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> String
label resistor =
  let o = ohms resistor
  in unitLabel o "ohms"

ohms :: Resistor -> Int
ohms (Resistor (tensC, onesC, powerC)) =
  let (tens, ones, power) = (colorNum tensC, colorNum onesC, colorNum powerC)
      initial = tens*10 + ones
      multipilier = 10^power
  in initial * multipilier

colorNum :: Color -> Int
colorNum c = case c of
               Black -> 0
               Brown -> 1
               Red -> 2
               Orange -> 3
               Yellow -> 4
               Green -> 5
               Blue -> 6
               Violet -> 7
               Grey -> 8
               White -> 9

prefixes :: [(Int, String)]
prefixes = [ (1_000_000_000, "giga")
           , (1_000_000, "mega")
           , (1_000, "kilo")
           , (1, "")
           ]

unitLabel :: Int -> String -> String
unitLabel num unit =
  let match (power, pre) =
        if num >= power
        then Just (num `div` power, pre)
        else Nothing
      maybePrefix = listToMaybe . mapMaybe match $ prefixes
      (num', prefix) = fromMaybe (0, "") maybePrefix
  in show num' ++ " " ++ prefix ++ unit
