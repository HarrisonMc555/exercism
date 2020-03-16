module RNATranscription exposing (toRNA)


type DnaBase
    = DnaG
    | DnaC
    | DnaT
    | DnaA


type RnaBase
    = RnaC
    | RnaG
    | RnaA
    | RnaU


toRNA : String -> Result Char String
toRNA dnaString =
    let
        dnaList =
            String.toList dnaString

        parse =
            maybeToResultId charToDnaBase

        dnaResult =
            collectResult parse dnaList

        process =
            dnaToRna >> List.map rnaBaseToChar >> String.fromList
    in
    Result.map process dnaResult


dnaToRna : List DnaBase -> List RnaBase
dnaToRna =
    List.map dnaBaseToRnaBase


dnaBaseToRnaBase : DnaBase -> RnaBase
dnaBaseToRnaBase dna =
    case dna of
        DnaG ->
            RnaC

        DnaC ->
            RnaG

        DnaT ->
            RnaA

        DnaA ->
            RnaU


charToDnaBase : Char -> Maybe DnaBase
charToDnaBase char =
    case Char.toUpper char of
        'G' ->
            Just DnaG

        'C' ->
            Just DnaC

        'T' ->
            Just DnaT

        'A' ->
            Just DnaA

        _ ->
            Nothing


rnaBaseToChar : RnaBase -> Char
rnaBaseToChar rna =
    case rna of
        RnaC ->
            'C'

        RnaG ->
            'G'

        RnaA ->
            'A'

        RnaU ->
            'U'



-- Helpers


maybeToResultId : (a -> Maybe b) -> a -> Result a b
maybeToResultId function a =
    Result.fromMaybe a <| function a


collectResult : (a -> Result e b) -> List a -> Result e (List b)
collectResult function list =
    case list of
        first :: rest ->
            case function first of
                Ok x ->
                    Result.map ((::) x) <|
                        collectResult function rest

                Err e ->
                    Err e

        [] ->
            Ok []
