module ArmstrongNumbers exposing (isArmstrongNumber)


isArmstrongNumber : Int -> Bool
isArmstrongNumber num =
    num == armstrongSum num


armstrongSum : Int -> Int
armstrongSum num =
    let
        digits =
            getDigits num

        numDigits =
            List.length digits

        poweredDigits =
            List.map (\x -> x ^ numDigits) digits
    in
    List.sum poweredDigits


getDigits : Int -> List Int
getDigits x =
    if x <= 0 then
        []

    else
        modBy 10 x :: getDigits (x // 10)
