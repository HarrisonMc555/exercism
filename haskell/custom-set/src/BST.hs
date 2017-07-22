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
    , member
    , delete
    , showPretty
    ) where

-- Modified to disallow duplicates and implement Foldable
data BST a = BST (BST a) a (BST a)
           | Nil
           deriving (Show)

instance Eq a => Eq (BST a) where
  setA == setB = toList setA == toList setB

instance Foldable BST where
  foldr f z (BST l x r) = foldr f (f x (foldr f z r)) l
  foldr _ z Nil = z

instance Functor BST where
  fmap f (BST l x r) = BST (fmap f l) (f x) (fmap f r)
  fmap _ Nil = Nil

instance Traversable BST where
  traverse f (BST l x r) = BST <$> traverse f l <*> f x <*> traverse f r
  traverse _ Nil = pure Nil

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
insert x b@(BST left y right)
  | x < y = BST (insert x left) y right
  | x > y  = BST left y (insert x right)
  | otherwise = b
insert x Nil = singleton x

singleton :: a -> BST a
singleton x = BST empty x empty

toList :: BST a -> [a]
toList (BST left y right) = toList left ++ [y] ++ toList right
toList Nil = []

member :: Eq a => a -> BST a -> Bool
member x (BST l y r) = x == y || member x l || member x r
member _ Nil = False

delete :: Ord a => a -> BST a -> BST a
delete x (BST l y r)
  | x < y = BST (delete x l) y r
  | x > y = BST l y (delete x r)
  | otherwise = rearrange l r
  where rearrange Nil r' = r'
        rearrange l' Nil = l'
        rearrange l' r' = let maxl = findMax l' in
          BST (delete maxl l') maxl r'
        findMax (BST _ _ r'@BST {}) = findMax r'
        findMax (BST _ x' Nil) = x'
        findMax Nil = error "This should never happen"
delete _ Nil = Nil

showPretty :: Show a => BST a -> String
showPretty = go 0
  where go n (BST l x r) = go (n+1) l ++
                           replicate n ' ' ++ show x ++ "\n" ++
                           go (n+1) r
        go n Nil = replicate n ' ' ++ ".\n"
