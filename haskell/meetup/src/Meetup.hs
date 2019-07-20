module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.List (find)
import Data.Maybe (fromJust)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Enum, Eq)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  fromJust $ find (isWeekday weekday) scheduleWeek
  where scheduleWeek = getWeek schedule year month

isWeekday :: Weekday -> Day -> Bool
isWeekday weekday day = weekday == toWeekday day

toWeekday :: Day -> Weekday
toWeekday day = let (_, _, weekday) = toWeekDate day
                in toEnum (weekday - 1)

getWeek :: Schedule -> Integer -> Int -> [Day]
getWeek schedule = case schedule of
                     First  -> daysFrom 1
                     Second -> daysFrom 8
                     Third  -> daysFrom 15
                     Fourth -> daysFrom 22
                     Last   -> lastDays
                     Teenth -> daysFrom 13

daysFrom :: Int -> Integer -> Int -> [Day]
daysFrom first = days [first..first+6]

lastDays :: Integer -> Int -> [Day]
lastDays year month = days [lastDay-6..lastDay] year month
  where lastDay = gregorianMonthLength year month

days :: [Int] -> Integer -> Int -> [Day]
days nums year month = map (fromGregorian year month) nums