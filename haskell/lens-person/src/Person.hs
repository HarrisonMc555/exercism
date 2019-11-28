{-# LANGUAGE TemplateHaskell #-}

module Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  ) where

import Control.Arrow ((>>>))
import Control.Lens (makeLenses, (^.), (.~), over, mapped)
-- import Data.Time.Calendar (Day)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Lens (months)

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     }
              deriving (Show)

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 }
              deriving (Show)

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 }
              deriving (Show)

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       }
              deriving (Show)

makeLenses ''Person
makeLenses ''Name
makeLenses ''Born
makeLenses ''Address

bornStreet :: Born -> String
bornStreet = (^.bornAt.street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet newStreet = address.street .~ newStreet

setBirthMonth :: Int -> Person -> Person
setBirthMonth newMonth = born.bornOn.months .~ newMonth

renameStreets :: (String -> String) -> Person -> Person
renameStreets f = over (address.street) f . over (born.bornAt.street) f
