module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)
import qualified BST
import Data.Maybe (isNothing)

newtype CustomSet a = CustomSet { getBST :: BST.BST a } deriving (Show)

instance Eq a => Eq (CustomSet a) where
  setA == setB = getBST setA == getBST setB

instance Functor CustomSet where
  fmap f = CustomSet . fmap f . getBST

instance Foldable CustomSet where
  foldr f b = foldr f b . getBST

fromBST :: BST.BST a -> CustomSet a
fromBST = CustomSet

delete :: Ord a => a -> CustomSet a -> CustomSet a
delete x = fromBST . BST.delete x . getBST

difference :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
difference = foldr delete

empty :: CustomSet a
empty = fromBST BST.empty

fromList :: Ord a => [a] -> CustomSet a
fromList = fromBST . BST.fromList

insert :: Ord a => a -> CustomSet a -> CustomSet a
insert x = fromBST . BST.insert x . getBST

intersection :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
intersection setA = foldr f empty
  where f x set = if member x setA
                  then insert x set
                  else set

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = foldr f True setA
  where f x b = b && not (member x setB)

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = foldr f True setA
  where f x b = b && member x setB

member :: Eq a => a -> CustomSet a -> Bool
member x = BST.member x . getBST

null :: CustomSet a -> Bool
null = isNothing . BST.bstValue . getBST

size :: CustomSet a -> Int
size = sum . fmap (const 1)

toList :: CustomSet a -> [a]
toList = BST.toList . getBST

union :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
union = foldr insert
