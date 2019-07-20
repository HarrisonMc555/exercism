module Clock (clockHour, clockMin, fromHourMin, toString) where

import Text.Printf (printf)

newtype Clock = Clock { getClock :: Int }
  deriving (Eq, Ord)

instance Bounded Clock where
  minBound = Clock 0
  maxBound = Clock (maxMinutes * maxHours)

mkClock :: Int -> Clock
mkClock x = Clock (x `mod` getClock maxBound)

instance Num Clock where
  (Clock c1) + (Clock c2) = mkClock (c1 + c2)
  (Clock c1) - (Clock c2) = mkClock (c1 - c2)
  negate (Clock c) = mkClock (negate c)
  fromInteger = mkClock . fromIntegral
  -- These operations don't make sense for Clocks (Clock really doesn't belong
  -- in the Num class, but that's the way it's being tested)
  _ * _ = error "can't multiply Clocks"
  abs _ = error "can't take absolute value of Clocks"
  signum _ = error "can't make signum of Clocks"

instance Show Clock where
  show c = printf "%02d:%02d" hour minute
    where minute = clockMin c
          hour = clockHour c
  
maxMinutes :: Int
maxMinutes = 60

maxHours :: Int
maxHours = 24

clockHour :: Clock -> Int
clockHour = (`div` maxMinutes) . getClock

clockMin :: Clock -> Int
clockMin = (`mod` maxMinutes) . getClock

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = mkClock $ (maxMinutes * hour) + minute

toString :: Clock -> String
toString = show