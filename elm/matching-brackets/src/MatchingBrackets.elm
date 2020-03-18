module MatchingBrackets exposing (isPaired)

import Dict exposing (Dict)
import Set exposing (Set)


isPaired : String -> Bool
isPaired input =
    isPairedHelper (String.toList input) []


isPairedHelper : List Char -> List Char -> Bool
isPairedHelper chars stack =
    case chars of
        char :: restChars ->
            if isClosing char then
                case stack of
                    stackClosingChar :: restStack ->
                        if stackClosingChar == char then
                            isPairedHelper restChars restStack

                        else
                            False

                    [] ->
                        False

            else
                case openToClose char of
                    Just close ->
                        isPairedHelper restChars (close :: stack)

                    Nothing ->
                        isPairedHelper restChars stack

        [] ->
            List.isEmpty stack


openToClose : Char -> Maybe Char
openToClose c =
    Dict.get c bracketPairs


isClosing : Char -> Bool
isClosing c =
    Set.member c closingBrackets


bracketPairs : Dict Char Char
bracketPairs =
    [ ( '(', ')' )
    , ( '[', ']' )
    , ( '{', '}' )
    ]
        |> Dict.fromList


closingBrackets : Set Char
closingBrackets =
    Dict.values bracketPairs
        |> Set.fromList
