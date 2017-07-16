module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = BST (BST a) a (BST a)
           | Nil
           deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft (BST left _ _) = Just left
bstLeft Nil = Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight (BST _ _ right) = Just right
bstRight Nil = Nothing

bstValue :: BST a -> Maybe a
bstValue (BST _ x _) = Just x
bstValue Nil = Nothing

empty :: BST a
empty = Nil

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) Nil

insert :: Ord a => a -> BST a -> BST a
insert x (BST left value right) =
  if x <= value
  then BST (insert x left) value right
  else BST left value (insert x right)
insert x Nil = singleton x

singleton :: a -> BST a
singleton x = BST Nil x Nil

toList :: BST a -> [a]
toList (BST left value right) = toList left ++ [value] ++ toList right
toList Nil = []
