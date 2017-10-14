module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.List (nub)
import Control.Monad (guard)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Enum, Bounded, Show)

data Color = Red | Yellow | Green | Blue | Ivory
  deriving (Eq, Enum, Bounded, Show)

data Drink = Coffee | Milk | OrangeJuice | Tea | Water
  deriving (Eq, Enum, Bounded, Show)

data Smoke = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Enum, Bounded, Show)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Enum, Bounded, Show)

data Location = First | Second | Third | Fourth | Fifth
  deriving (Eq, Enum, Bounded, Show)

data House = House { resident :: Resident
                 , color    :: Color
                 , drink    :: Drink
                 , smoke    :: Smoke
                 , pet      :: Pet
                 , location :: Location
                 } deriving (Eq, Show)

solve :: Solution
solve = Solution { waterDrinker = waterDrinker'
                 , zebraOwner   = zebraOwner'
                 }
  where waterDrinker' = residentWith drink Water
        zebraOwner'   = residentWith pet Zebra
        residentWith what value = resident $ houseWith what value solution

solution :: [House]
solution = head $ do
  first <- housesAt First
  second <- housesAt Second
  guard $ uniqueHouses [first, second]
  third <- housesAt Third
  guard $ uniqueHouses [first, second, third]
  fourth <- housesAt Fourth
  guard $ uniqueHouses [first, second, third, fourth]
  fifth <- housesAt Fifth
  let fiveHouses = [first, second, third, fourth, fifth]
  guard $ uniqueHouses fiveHouses
  guard $ isValidLocations fiveHouses
  return fiveHouses
  where housesAt location' = filter ((location' ==) . location) validHouses

isValidHouse :: House -> Bool
isValidHouse (House resident' color' drink' smoke' pet' location') =
  all (uncurry iff) [ (resident' == Englishman, color' == Red)
                    , (resident' == Spaniard, pet' == Dog)
                    , (drink' == Coffee, color' == Green)
                    , (resident' == Ukrainian, drink' == Tea)
                    , (smoke' == OldGold, pet' == Snails)
                    , (smoke' == Kools, color' == Yellow)
                    , (drink' == Milk, location' == Third)
                    , (resident' == Norwegian, location' == First)
                    , (smoke' == LuckyStrike, drink' == OrangeJuice)
                    , (resident' == Japanese, smoke' == Parliaments)
                    ]

isValidLocations :: [House] -> Bool
isValidLocations houses =
  and [ houseWith' color Green `toTheRightOf` houseWith' color Ivory
      , houseWith' smoke Chesterfields `isNextTo` houseWith' pet Fox
      , houseWith' smoke Kools `isNextTo` houseWith' pet Horse
      , houseWith' resident Norwegian `isNextTo` houseWith' color Blue
      ]
  where h1 `toTheRightOf` h2 = indexOf h1 == indexOf h2 - 1
        h1 `isNextTo` h2 = abs (indexOf h1 - indexOf h2) == 1
        houseWith' what value = houseWith what value houses
        indexOf h = fromEnum $ location h

uniqueHouses :: [House] -> Bool
uniqueHouses houses =
  unique' resident && unique' color && unique' drink &&
  unique' smoke && unique' pet -- && unique' location
  where unique' f = uniqueWith f houses

validHouses :: [House]
validHouses = filter isValidHouse allHouses

allHouses :: [House]
allHouses = do
  resident' <- enumAll
  color'    <- enumAll
  drink'    <- enumAll
  smoke'    <- enumAll
  pet'      <- enumAll
  location' <- enumAll
  return $ House resident' color' drink' smoke' pet' location'

houseWith :: Eq a => (House -> a) -> a -> [House] -> House
houseWith what value = fromJust . find ((== value) . what)

-- Helper Functions

enumAll :: (Enum a, Bounded a) => [a]
enumAll = [minBound..maxBound]

iff :: Bool -> Bool -> Bool
iff True  True  = True
iff False False = True
iff _     _     = False

uniqueWith :: (Eq b) => (a -> b) -> [a] -> Bool
uniqueWith f = unique . map f

unique :: (Eq a) => [a] -> Bool
unique xs = length xs == (length . nub) xs
