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
  deriving (Eq, Show, Read)

value :: [Color] -> Int
value = digitsToInt . map colorNum

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

digitsToInt :: [Int] -> Int
digitsToInt =
  let addDigit acc x = 10*acc + x
  in foldl addDigit 0
