module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = (div4 && not div100) || div400
  where div4 = year `mod` 4 == 0
        div100 = year `mod` 100 == 0
        div400 = year `mod` 400 == 0
