module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose)
import Control.Monad (replicateM)

data Character = Character
  { name         :: String
  , strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier value = (value - 10) `div` 2

ability :: Gen Int
ability = do
  dice <- replicateM 4 $ choose (1, 6)
  let smallestDie = minimum dice
  return $ sum dice - smallestDie

character :: Gen Character
character =
  error "You need to implement this generator."
