module Sublist exposing (ListComparison(..), sublist)


type ListComparison
    = Equal
    | Superlist
    | Sublist
    | Unequal


sublist : List a -> List a -> ListComparison
sublist alist blist =
    let
        alen =
            List.length alist

        blen =
            List.length blist

        ( matches, response ) =
            case compare alen blen of
                LT ->
                    ( containedBy alist blist, Sublist )

                EQ ->
                    ( alist == blist, Equal )

                GT ->
                    ( containedBy blist alist, Superlist )
    in
    if matches then
        response

    else
        Unequal


containedBy : List a -> List a -> Bool
containedBy inside outside =
    let
        insideLen =
            List.length inside

        outsideLen =
            List.length outside
    in
    if outsideLen < insideLen then
        False

    else if List.take insideLen outside == inside then
        True

    else
        List.tail outside
            |> Maybe.map (containedBy inside)
            |> Maybe.withDefault False
