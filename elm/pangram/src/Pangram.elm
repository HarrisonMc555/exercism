module Pangram exposing (isPangram)

import Set


isPangram : String -> Bool
isPangram sentence =
    let
        letters =
            sentence
                |> String.toList
                |> List.map Char.toLower
                |> Set.fromList

        sentenceContains c =
            Set.member c letters
    in
    List.all sentenceContains alphabet


alphabet : List Char
alphabet =
    charRange 'a' 'z'


charRange : Char -> Char -> List Char
charRange from to =
    List.map Char.fromCode <| List.range (Char.toCode from) (Char.toCode to)
