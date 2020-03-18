module Dominoes (chain) where

import Data.List (uncons)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain dominoes = do (d, rest) <- uncons dominoes
                    return []
