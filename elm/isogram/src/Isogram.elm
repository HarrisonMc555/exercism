module Isogram exposing (isIsogram)

import Set


isIsogram : String -> Bool
isIsogram sentence =
    let
        letters =
            sentence
                |> String.toList
                |> List.map Char.toLower
                |> List.filter Char.isAlpha
    in
    List.length letters == Set.size (Set.fromList letters)
