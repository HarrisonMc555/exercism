module RailFenceCipher (encode, decode) where

encode :: Int -> String -> String
encode = error "You need to implement this function!"

decode :: Int -> String -> String
decode = error "You need to implement this function!"

railNums :: Int -> Int -> (Int, Int, Int)
railNums numRails index =
  let cycleLength = numRails * 2 - 2
      numLeadingSpaces = index
      numTrailingSpaces = numLeadingSpaces - 1
      numMiddleSpaces = cycleLength - numLeadingSpaces - numTrailingSpaces
  in (numLeadingSpaces, numMiddleSpaces, numTrailingSpaces)


{-

1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

1   1   1   1   1   1   1   1   1
 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
  3   3   3   3   3   3   3   3 

1     1     1     1     1     1
 2   2 2   2 2   2 2   2 2   2
  3 3   3 3   3 3   3 3   3 3 
   4     4     4     4     4  

1       1       1       1       1
 2     2 2     2 2     2 2     2
  3   3   3   3   3   3   3   3 
   4 4     4 4     4 4     4 4  
    5       5       5       5   

_1234567  _1234567  _1234567  _1234567
1_12345_  1_12345_  1_12345_  1_12345_
12_123_1  12_123_1  12_123_1  12_123_1
123_1_12  123_1_12  123_1_12  123_1_12
1234_123  1234_123  1234_123  1234_123

2*(n-2) + 2

2n - 4 + 2
2n - 2

2 -> 2
3 -> 4
4 -> 6
5 -> 8

-}
