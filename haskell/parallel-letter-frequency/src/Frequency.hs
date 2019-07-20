module Frequency (frequency) where

import Data.Map  (Map, insertWith, empty, unionWith)
import Data.Text (Text, foldr)
import Data.Char (toLower, isAlpha)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (Strategy, parMap, using, rdeepseq)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers = mapReduce rdeepseq oneFrequency rdeepseq unionAdd

oneFrequency :: Text -> Map Char Int
oneFrequency = Data.Text.foldr increment empty

increment :: Char -> Map Char Int -> Map Char Int
increment c m = if isAlpha c
                then insertWith (+) (toLower c) 1 m
                else m

unionAdd :: (Ord k, Num v) => [Map k v] -> Map k v
unionAdd = Prelude.foldr (unionWith (+)) empty

-- http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html
mapReduce
    :: Strategy b    -- evaluation strategy for mapping
    -> (a -> b)      -- map function
    -> Strategy c    -- evaluation strategy for reduction
    -> ([b] -> c)    -- reduce function
    -> [a]           -- list to map over
    -> c
mapReduce mapStrat mapFunc reduceStrat reduceFunc input =
    mapResult `pseq` reduceResult
  where mapResult    = parMap mapStrat mapFunc input
        reduceResult = reduceFunc mapResult `using` reduceStrat

-- Sequential
_frequency :: Int -> [Text] -> Map Char Int
_frequency nWorkers = Prelude.foldr addText empty
  where addText t m = Data.Text.foldr increment m t