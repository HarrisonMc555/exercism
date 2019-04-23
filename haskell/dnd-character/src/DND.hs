module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen)

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
ability =
  error "You need to implement this generator."

character :: Gen Character
character =
  error "You need to implement this generator."
