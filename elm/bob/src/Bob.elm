module Bob exposing (hey)

import List.Extra
import Maybe.Extra


hey : String -> String
hey remark =
    Maybe.withDefault "Whatever." <| Maybe.Extra.oneOf matchers remark


matchers : List (String -> Maybe String)
matchers =
    matchPairs |> List.map runMatch


matchPairs : List ( String -> Bool, String )
matchPairs =
    [ ( isYellingQuestion, "Calm down, I know what I'm doing!" )
    , ( isYelling, "Whoa, chill out!" )
    , ( isQuestion, "Sure." )
    , ( isSilence, "Fine. Be that way!" )
    ]


runMatch : ( a -> Bool, b ) -> a -> Maybe b
runMatch ( f, output ) input =
    if f input then
        Just output

    else
        Nothing


isQuestion : String -> Bool
isQuestion string =
    let
        nonWsChars =
            String.toList string |> List.filter (not << isWhitespace)

        lastNonWsChar =
            List.Extra.last nonWsChars
    in
    lastNonWsChar == Just '?'


isYelling : String -> Bool
isYelling string =
    let
        letters =
            String.toList string |> List.filter Char.isAlpha

        allUpper =
            List.all Char.isUpper letters

        hasLetters =
            not <| List.isEmpty letters
    in
    hasLetters && allUpper


isYellingQuestion : String -> Bool
isYellingQuestion string =
    isYelling string && isQuestion string


isSilence : String -> Bool
isSilence string =
    List.all isWhitespace <| String.toList string


isWhitespace : Char -> Bool
isWhitespace char =
    case char of
        ' ' ->
            True

        '\t' ->
            True

        '\n' ->
            True

        -- For some reason elm-format wants '\r' written this way ¯\_(ツ)_/¯
        '\u{000D}' ->
            True

        _ ->
            False
