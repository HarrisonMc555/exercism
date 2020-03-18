module Zipper
 ( BinTree(BT)
 , fromTree
 , left
 , right
 , setLeft
 , setRight
 , setValue
 , toTree
 , up
 , value
 ) where

import Data.List (uncons)

data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

data Zipper a = Zipper { tree :: BinTree a
                       , crumbs :: Breadcrumbs a
                       }
                deriving (Eq, Show)

type Breadcrumbs a = [Crumb a]

data Crumb a = LeftCrumb a (Maybe (BinTree a))
             | RightCrumb a (Maybe (BinTree a))
             deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree t = Zipper t []

toTree :: Zipper a -> BinTree a
toTree = tree . lastJust up

value :: Zipper a -> a
value = btValue . tree

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT x l r) cs) =
  let addLeft l' = Zipper l' $ (LeftCrumb x r) : cs
  in addLeft <$> l

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT x l r) cs) =
  let addRight r' = Zipper r' $ (RightCrumb x l) : cs
  in addRight <$> r

up :: Zipper a -> Maybe (Zipper a)
up (Zipper prev cs) =
  let up' (c, cs') =
        let t = case c of
                  LeftCrumb x r -> (BT x (Just prev) r)
                  RightCrumb x l -> (BT x l (Just prev))
        in Zipper t cs'
  in up' <$> uncons cs

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) cs) = Zipper (BT x l r) cs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l (Zipper (BT x _ r) cs) = Zipper (BT x l r) cs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r (Zipper (BT x l _) cs) = Zipper (BT x l r) cs

-- Helpers

lastJust :: (a -> Maybe a) -> a -> a
lastJust f x = lastJust' x (f x)
  where lastJust' a b = case b of
                            (Just b') -> lastJust' b' (f b')
                            Nothing -> a

-- Debug
_printBT :: Show a => BinTree a -> IO ()
_printBT = putStr . unlines . _prettyBT

_prettyBT :: Show a => BinTree a -> [String]
_prettyBT = go 0
  where go indent t =
          [valueHeader, leftHeader] ++ leftTree ++ [rightHeader] ++ rightTree
          where indentString = replicate indent ' '
                valueHeader = indentString ++ "value: " ++ show (btValue t)
                leftHeader = indentString ++ "left:"
                rightHeader = indentString ++ "right"
                leftTree = maybe defaultTree (go nextIndent) (btLeft t)
                rightTree = maybe defaultTree (go nextIndent) (btRight t)
                defaultTree = [indentString ++ "N/A"]
                nextIndent = indent + 1

-- _leaf v     = _node v Nothing Nothing
-- _node v l r = Just (BT v l r :: BinTree Int)
-- _t1         = BT 1 (_node 2 Nothing  $ _leaf 3) $ _leaf 4
-- _t2         = BT 1 (_node 5 Nothing  $ _leaf 3) $ _leaf 4
-- _t3         = BT 1 (_node 2 (_leaf 5) $ _leaf 3) $ _leaf 4
-- _t4         = BT 1 (_leaf 2                  ) $ _leaf 4
-- _t5         = BT 6 (_leaf 7                  ) $ _leaf 8
-- _t6         = BT 1 (_node 2 Nothing  $ _leaf 3) $ _node 6 (_leaf 7) (_leaf 8)
-- _t7         = BT 1 (_node 2 Nothing  $ _leaf 5) $ _leaf 4
