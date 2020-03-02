module Hamming exposing (distance)

import List.Extra


distance : String -> String -> Result String Int
distance left right =
    let
        leftChars =
            String.toList left

        rightChars =
            String.toList right
    in
    if String.length left /= String.length right then
        Err "left and right strands must be of equal length"

    else
        List.Extra.zip leftChars rightChars
            |> List.Extra.count (\( a, b ) -> a /= b)
            |> Ok
