module FoodChain (song) where

import Data.List (tails, intercalate)

type Animal = String
type Description = String
type Extra = String
data Prey = SimplePrey Animal Description
          | ComplexPrey Animal Description Extra
          deriving (Show)

prey :: [Prey]
prey = [ SimplePrey "fly"     "I don't know why she swallowed the fly. Perhaps she'll die."
       , ComplexPrey "spider" "It wriggled and jiggled and tickled inside her." " that wriggled and jiggled and tickled inside her"
       , SimplePrey "bird"    "How absurd to swallow a bird!"
       , SimplePrey "cat"     "Imagine that, to swallow a cat!"
       , SimplePrey "dog"     "What a hog, to swallow a dog!"
       , SimplePrey "goat"    "Just opened her throat and swallowed a goat!"
       , SimplePrey "cow"     "I don't know how she swallowed a cow!"
       , SimplePrey "horse"   "She's dead, of course!"
       ]

song :: String
song = (intercalate "\n" . map (unlines . correctVerse) . zip [1..]) preyGroups
-- song = (unlines . map (unlines . correctVerse) . zip [1..]) preyGroups
  where preyGroups = drop 1 . reverse . tails . reverse $ prey
        correctVerse (n, as)
          | n == 1 = firstVerse as
          | n == maxN = lastVerse as
          | otherwise = normalVerse as
        maxN = length prey

firstVerse :: [Prey] -> [String]
firstVerse [a] = firstLines a
firstVerse _ = error "empty first lines"

normalVerse :: [Prey] -> [String]
normalVerse [] = [""]
normalVerse ps@(p:_) = firstLines p ++
                       map swallowedLine (pairs ps) ++
                       perhapsShellDie

lastVerse :: [Prey] -> [String]
lastVerse [] = error "empty last lines"
lastVerse (p:_) = firstLines p

pairs ::[a] -> [(a, a)]
pairs (x:x':xs) = (x, x') : pairs (x':xs)
pairs _ = []

swallowedLine :: (Prey, Prey) -> String
swallowedLine (a, b) = "She swallowed the " ++
                       animal a ++
                       " to catch the " ++
                       animal b ++
                       extra b ++
                       "."

firstLines :: Prey -> [String]
firstLines a =
  ["I know an old lady who swallowed a " ++ animal a ++ ".", description a]

perhapsShellDie :: [String]
perhapsShellDie =
  ["I don't know why she swallowed the fly. Perhaps she'll die."]

animal :: Prey -> String
animal (SimplePrey a _) = a
animal (ComplexPrey a _ _) = a

description :: Prey -> String
description (SimplePrey _ d) = d
description (ComplexPrey _ d _) = d

extra :: Prey -> String
extra (SimplePrey _ _) = ""
extra (ComplexPrey _ _ e) = e