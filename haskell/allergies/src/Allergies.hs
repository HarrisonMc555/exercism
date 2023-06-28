module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Enum, Bounded, Show)

allergies :: Int -> [Allergen]
allergies score = filter (`isAllergicTo` score) [minBound .. maxBound]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = testBit score (fromEnum allergen)