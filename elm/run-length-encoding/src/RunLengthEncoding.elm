module RunLengthEncoding exposing (decode, encode)

import List.Extra as ListE


encode : String -> String
encode =
    String.toList
        >> ListE.group
        >> List.map encodeGroup
        >> String.concat


decode : String -> String
decode =
    String.toList
        >> ListE.unfoldr decodeGroup
        >> String.concat


encodeGroup : ( Char, List Char ) -> String
encodeGroup ( letter, rest ) =
    if List.isEmpty rest then
        String.fromChar letter

    else
        let
            num =
                List.length rest + 1
        in
        String.fromInt num ++ String.fromChar letter


decodeGroup : List Char -> Maybe ( String, List Char )
decodeGroup chars =
    let
        ( digitChars, afterDigitChars ) =
            ListE.span Char.isDigit chars

        num =
            digitChars
                |> String.fromList
                |> String.toInt
                |> Maybe.withDefault 1

        getGroup ( letter, afterLetterChars ) =
            let
                s =
                    letter
                        |> List.repeat num
                        |> String.fromList
            in
            ( s, afterLetterChars )
    in
    afterDigitChars
        |> ListE.uncons
        |> Maybe.map getGroup
