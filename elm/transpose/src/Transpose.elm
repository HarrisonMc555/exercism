module Transpose exposing (transpose)

import List.Extra


transpose : List String -> List String
transpose lines =
    lines
        |> List.map String.toList
        |> transposeGrid ' '
        |> List.map (trimTrailingSpaces >> String.fromList)


transposeGrid : a -> List (List a) -> List (List a)
transposeGrid default grid =
    let
        numRows =
            List.length grid

        numCols =
            List.map List.length grid
                |> List.maximum
                |> Maybe.withDefault 0

        getCol index =
            List.range 0 (numRows - 1)
                |> List.map (\row -> List.Extra.getAt row grid)
                |> List.map (getElement index)

        getElement index maybeRow =
            Maybe.andThen (List.Extra.getAt index) maybeRow
                |> Maybe.withDefault default
    in
    List.range 0 (numCols - 1)
        |> List.map getCol


trimTrailingSpaces : List Char -> List Char
trimTrailingSpaces chars =
    let
        numToKeep =
            findRevIndex ((==) ' ') chars
                |> Maybe.withDefault (List.length chars)
    in
    List.take numToKeep chars


findRevIndex : (a -> Bool) -> List a -> Maybe Int
findRevIndex pred list =
    let
        reversed =
            List.reverse list

        revIndex =
            List.Extra.findIndex pred reversed
    in
    revIndex
        |> Maybe.map (\i -> List.length list - i - 1)
