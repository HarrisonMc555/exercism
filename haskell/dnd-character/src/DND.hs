module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose, vectorOf, resize, listOf, arbitraryASCIIChar)

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
  dice <- vectorOf 4 $ choose (1, 6)
  let smallestDie = minimum dice
  return $ sum dice - smallestDie

character :: Gen Character
character = do
  name' <- nameGen
  abilities <- vectorOf 6 ability
  let [ strength'
        , dexterity'
        , constitution'
        , intelligence'
        , wisdom'
        , charisma'
        ] = abilities
  let hitpoints' = 10 + modifier constitution'
  return $ Character { name=name'
                     , strength=strength'
                     , dexterity=dexterity'
                     , constitution=constitution'
                     , intelligence=intelligence'
                     , wisdom=wisdom'
                     , charisma=charisma'
                     , hitpoints=hitpoints'
                     }

nameGen :: Gen String
nameGen = do
  beginning <- vectorOf 3 arbitraryASCIIChar
  end <- resize 17 $ listOf arbitraryASCIIChar
  return $ beginning ++ end
