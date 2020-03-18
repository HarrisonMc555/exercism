module Anagram exposing (detect)

import Bag exposing (Bag)


detect : String -> List String -> List String
detect word candidates =
    let
        wordLetters =
            wordBag word

        sameLetters otherWord =
            wordBag otherWord == wordLetters

        notSame otherWord =
            not <| equalsCaseInsensitive word otherWord

        keep otherWord =
            notSame otherWord && sameLetters otherWord
    in
    List.filter keep candidates


equalsCaseInsensitive : String -> String -> Bool
equalsCaseInsensitive string1 string2 =
    let
        toLower =
            String.toList >> List.map Char.toLower
    in
    toLower string1 == toLower string2


wordBag : String -> Bag Char
wordBag =
    String.toList >> List.map Char.toLower >> listToBag


listToBag : List comparable -> Bag comparable
listToBag =
    List.foldl (Bag.insert 1) Bag.empty
