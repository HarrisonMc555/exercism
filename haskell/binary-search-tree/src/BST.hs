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
fromList = foldl (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert x (BST left y right)
  | x <= y = BST (insert x left) y right
  | otherwise  = BST left y (insert x right)
insert x Nil = singleton x

singleton :: a -> BST a
singleton x = BST empty x empty

toList :: BST a -> [a]
toList (BST left y right) = toList left ++ [y] ++ toList right
toList Nil = []
