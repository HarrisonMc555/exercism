module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

newtype Clock = Clock { getClock :: Int }
  deriving (Eq, Ord)

mkClock :: Int -> Clock
mkClock x = Clock $ x `mod` (minutesPerHour * hoursPerDay)

instance Show Clock where
  show c = printf "%02d:%02d" hour minute
    where minute = clockMin c
          hour = clockHour c

addDelta :: Int -> Int -> Clock -> Clock
addDelta hours minutes clock =
  mkClock $ hours * minutesPerHour + minutes + getClock clock

minutesPerHour :: Int
minutesPerHour = 60

hoursPerDay :: Int
hoursPerDay = 24

clockHour :: Clock -> Int
clockHour = (`div` minutesPerHour) . getClock

clockMin :: Clock -> Int
clockMin = (`mod` minutesPerHour) . getClock

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = mkClock $ (minutesPerHour * hour) + minute

toString :: Clock -> String
toString = show