module ResistorColors (Color(..), value) where

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
  deriving (Eq, Show, Read, Enum, Bounded)

value :: (Color, Color) -> Int
value (tens, ones) = 
  colorNum tens * 10 + colorNum ones

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
