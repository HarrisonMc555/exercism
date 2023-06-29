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

import Control.Lens (makeLenses, (^.), (.~), over)
import Control.Lens.Type (Lens')
import Data.Time.Calendar (Day, toGregorian, fromGregorian)
-- import Data.Time.Lens (months)

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

months :: Lens' Day Int
months =
  lens getMonth setMonth
  where getMonth day =
          let (_, m, _) = toGregorian day
          in m
        setMonth day m =
          let (y, _, d) = toGregorian day
          in fromGregorian y m d

lens :: (Functor f) => (s -> a) -> (s -> a -> s) -> (a -> f a) -> s -> f s
lens getter setter f s = setter s <$> f (getter s)
