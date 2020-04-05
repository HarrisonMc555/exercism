module TwelveDays exposing (recite)


recite : Int -> Int -> List String
recite start stop =
    listSlice (start - 1) (stop - 1) verses


type alias VerseInfo =
    { ordinalNum : String
    , allItems : String
    }


type alias DayInfo =
    { ordinalNum : String
    , item : String
    }


verses : List String
verses =
    List.map verseToString verseInfos


verseToString : VerseInfo -> String
verseToString verseInfo =
    "On the "
        ++ verseInfo.ordinalNum
        ++ " day of Christmas my true love gave to me: "
        ++ verseInfo.allItems


verseInfos : List VerseInfo
verseInfos =
    let
        createVerseInfo ( day, rest ) =
            let
                items =
                    day.item
                        :: List.map .item rest
                        |> List.reverse
            in
            VerseInfo day.ordinalNum (joinItems items)
    in
    dayInfos
        |> List.reverse
        |> tailsNonEmpty
        |> List.map createVerseInfo
        |> List.reverse


dayInfos : List DayInfo
dayInfos =
    [ DayInfo "first" "a Partridge in a Pear Tree."
    , DayInfo "second" "two Turtle Doves"
    , DayInfo "third" "three French Hens"
    , DayInfo "fourth" "four Calling Birds"
    , DayInfo "fifth" "five Gold Rings"
    , DayInfo "sixth" "six Geese-a-Laying"
    , DayInfo "seventh" "seven Swans-a-Swimming"
    , DayInfo "eighth" "eight Maids-a-Milking"
    , DayInfo "ninth" "nine Ladies Dancing"
    , DayInfo "tenth" "ten Lords-a-Leaping"
    , DayInfo "eleventh" "eleven Pipers Piping"
    , DayInfo "twelfth" "twelve Drummers Drumming"
    ]


joinItems : List String -> String
joinItems items =
    case items of
        first :: second :: thirdOnward ->
            let
                rest =
                    second :: thirdOnward

                restString =
                    rest
                        |> List.reverse
                        |> List.map (\s -> s ++ ", ")
                        |> String.concat
            in
            restString ++ "and " ++ first

        _ ->
            String.concat items



-- Helpers


listSlice : Int -> Int -> List a -> List a
listSlice start stop =
    List.drop start >> List.take (stop - start + 1)


tailsNonEmpty : List a -> List ( a, List a )
tailsNonEmpty list =
    case list of
        first :: rest ->
            ( first, rest ) :: tailsNonEmpty rest

        [] ->
            []
