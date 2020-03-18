module ScrabbleScore exposing (scoreWord)

import Dict exposing (Dict)


scoreWord : String -> Int
scoreWord word =
    String.toList word
        |> List.map letterToScore
        |> List.sum


letterToScore : Char -> Int
letterToScore =
    Char.toUpper >> getFromDict letterScoreDict >> Maybe.withDefault 0


letterScoreList : List ( List Char, Int )
letterScoreList =
    [ ( [ 'A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T' ], 1 )
    , ( [ 'D', 'G' ], 2 )
    , ( [ 'B', 'C', 'M', 'P' ], 3 )
    , ( [ 'F', 'H', 'V', 'W', 'Y' ], 4 )
    , ( [ 'K' ], 5 )
    , ( [ 'J', 'X' ], 8 )
    , ( [ 'Q', 'Z' ], 10 )
    ]


letterScoreDict : Dict Char Int
letterScoreDict =
    let
        unpack ( chars, value ) =
            List.map (\c -> ( c, value )) chars
    in
    List.concatMap unpack letterScoreList
        |> Dict.fromList


getFromDict : Dict comparable v -> comparable -> Maybe v
getFromDict dict key =
    Dict.get key dict
