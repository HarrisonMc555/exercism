module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Node a (LinkedList a)
                  | Nil
                  deriving (Eq, Show)

instance Foldable LinkedList where
  foldMap f (Node a rest) = f a `mappend` foldMap f rest
  foldMap _ Nil           = mempty

datum :: LinkedList a -> a
datum (Node a _) = a
datum Nil        = error "empty linked list"

fromList :: [a] -> LinkedList a
fromList = foldr new nil

isNil :: LinkedList a -> Bool
isNil (Node _ _) = False
isNil Nil        = True

new :: a -> LinkedList a -> LinkedList a
new = Node

next :: LinkedList a -> LinkedList a
next (Node _ rest) = rest
next Nil           = error "empty linked list"

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = foldl (flip new) Nil

toList :: LinkedList a -> [a]
toList (Node a rest) = a : toList rest
toList Nil           = []
