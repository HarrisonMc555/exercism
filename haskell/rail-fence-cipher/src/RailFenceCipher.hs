module RailFenceCipher (encode, decode) where

import Data.Char (isSpace)

{-

rail letters

0    0 . . . ...  .   .   .   .   .  ...   .    .  2n-2   .    . ...
1    . 1 . . ...  .   .   .   .   .  ...   .  2n-3   .  2n-1   . ...
2    . . 2 . ...  .   .   .   .   .  ... 2n-4   .    .    .   2n ...
...          ...                     ...                         ...
n-3  . . . . ... n-3  .   .   .  n+1 ...   .    .    .    .    . ... 3n
n-2  . . . . ...  .  n-2  .   n   .  ...   .    .    .    .    . ...
n-1  . . . . ...  .   .  n-1  .   .  ...   .    .    .    .    . ...

period = 2n-2
offset = r, 2n-2-r

| # rails | period |
|--------:|-------:|
|       2 |      2 |
|       3 |      4 |
|       4 |      6 |
|       5 |      8 |
|     ... |    ... |
|       n |   2n-2 |

| index | offset 1 | offset 2 |
|------:|----------|---------:|
|     0 | 0        |      N/A |
|     1 | 1        |     2n-3 |
|     2 | 2        |     2n-4 |
|     3 | 3        |     2n-5 |
|   ... | ...      |      ... |
|   n-2 | n-2      |        n |
|   n-1 | n-1      |      N/A |

-}

encode :: Int -> String -> String
encode numRails = helper (replicate numRails "") 0 Down . removeSpaces
  where helper lines _ _ "" = concat lines
        helper lines index direction (letter:remaining) =
          let newLines = lines & element index %~ (++ [letter])
              (newIndex, newDirection) = nextIndexDirection index direction
          in helper newLines newIndex newDirection remaining
        nextIndexDirection index direction =
          case direction of
            Up -> if index == 0
                  then (1, Down)
                  else (index - 1, Up)
            Down -> if index == numRails - 1
              then (numRails - 2, Up)
              else (index + 1, Down)

decode :: Int -> String -> String
decode = error "You need to implement this function!"

data Direction = Down | Up deriving (Eq)

flipDirection :: Direction -> Direction
flipDirection Down = Up
flipDirection Up = Down

data ASetter a = ASetter { index :: Int }

element :: Int -> ASetter a
element = ASetter

removeSpaces :: String -> String
removeSpaces = filter (not . isSpace)

infixr 4 .~
set, (.~) :: ASetter a -> a -> [a] -> [a]
set setter e list =
  let i = index setter
  in take i list ++ [e] ++ drop (i + 1) list
(.~) = set

infixr 4 %~
over, (%~) :: ASetter a -> (a -> a) -> [a] -> [a]
over setter f list =
  let i = index setter
      e = list !! i
  in list & setter .~ f e
(%~) = over

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

