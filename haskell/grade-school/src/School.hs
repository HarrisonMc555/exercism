module School (School, add, empty, grade, sorted) where

import Data.Map (Map, member, alter, insert, toList)
import qualified Data.Map (empty, lookup)
import Data.List (sort)
import Data.Maybe (fromMaybe)

type School = Map Int [String]

add :: Int -> String -> School -> School
add gradeNum student school
  | member gradeNum school = alter (addStudent student) gradeNum school
  | otherwise              = insert gradeNum [student] school
  where addStudent s = fmap $ sort . (s:)

empty :: School
empty = Data.Map.empty

grade :: Int -> School -> [String]
grade gradeNum school = fromMaybe [] $ Data.Map.lookup gradeNum school

sorted :: School -> [(Int, [String])]
sorted school = sort $ toList school