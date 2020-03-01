module TwoFer exposing (twoFer)

import Maybe exposing (withDefault)


twoFer : Maybe String -> String
twoFer name =
    let
        nameString =
            withDefault "you" name
    in
    "One for " ++ nameString ++ ", one for me."
