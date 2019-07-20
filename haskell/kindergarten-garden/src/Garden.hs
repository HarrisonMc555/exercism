module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Map (Map, fromList)
import qualified Data.Map (lookup)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

defaultGarden :: String -> Map String [Plant]
defaultGarden = garden defaultStudents

garden :: [String] -> String -> Map String [Plant]
garden students plants = fromList $ zip students' plants'
  where plants'    = toPlants $ plantChars plants
        students'  = sort students
        toPlants   = map $ map $ fromJust . plant
        plantChars = transposeN plantsPerStudent . lines
        
lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants student = fromMaybe [] . Data.Map.lookup student

plant :: Char -> Maybe Plant
plant c = case c of 'C' -> Just Clover
                    'G' -> Just Grass
                    'R' -> Just Radishes
                    'V' -> Just Violets
                    _   -> Nothing

defaultStudents :: [String]
defaultStudents = [ "Alice"
                  , "Bob"
                  , "Charlie"
                  , "David"
                  , "Eve"
                  , "Fred"
                  , "Ginny"
                  , "Harriet"
                  , "Ileana"
                  , "Joseph"
                  , "Kincaid"
                  , "Larry"
                  ]

plantsPerStudent :: Int
plantsPerStudent = 2

transposeN :: Int -> [[b]] -> [[b]]
transposeN _ [] = []
transposeN n xs
  | all null xs = []
  | otherwise   = these : transposeN n rest
  where these = concatMap (take n) xs
        rest  = map (drop n) xs