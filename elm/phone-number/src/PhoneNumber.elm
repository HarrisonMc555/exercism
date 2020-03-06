module PhoneNumber exposing (getNumber)


getNumber : String -> Maybe String
getNumber phoneNumber =
    let
        digits =
            phoneNumber |> String.toList |> List.filter Char.isDigit

        numDigits =
            List.length digits

        cleanedDigits =
            case numDigits of
                11 ->
                    if List.head digits == Just '1' then
                        List.tail digits

                    else
                        Nothing

                10 ->
                    Just digits

                _ ->
                    Nothing
    in
    maybeFilter isValid10DigitNumber cleanedDigits |> Maybe.map String.fromList


isValid10DigitNumber : List Char -> Bool
isValid10DigitNumber digits =
    let
        firstAreaCodeDigit =
            getAt 0 digits

        firstExchangeDigit =
            getAt 3 digits
    in
    case ( firstAreaCodeDigit, firstExchangeDigit ) of
        ( Just d1, Just d2 ) ->
            isValidLeadingDigit d1 && isValidLeadingDigit d2

        _ ->
            False


isValidLeadingDigit : Char -> Bool
isValidLeadingDigit digit =
    case digit of
        '0' ->
            False

        '1' ->
            False

        _ ->
            True


getAt : Int -> List a -> Maybe a
getAt index list =
    if index < 0 then
        Nothing

    else
        List.head <| List.drop index list


maybeFilter : (a -> Bool) -> Maybe a -> Maybe a
maybeFilter f m =
    case Maybe.map f m of
        Just True ->
            m

        _ ->
            Nothing
