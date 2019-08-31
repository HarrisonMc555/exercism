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

import Prelude hiding (Left, Right)
import Data.Maybe (fromMaybe)

data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

data Zipper a = Zipper { zipTree :: BinTree a
                       , zipFocus :: BinTree a
                       , zipParent :: [BinTree a]
                       } deriving (Eq, Show)
-- data Zipper a = Zipper { zipTree   :: BinTree a
--                        , zipParent :: Maybe (Zipper a)
--                        } deriving (Eq, Show)
data Direction = Left | Right

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree tree []

toTree :: Zipper a -> BinTree a
toTree = zipTree

value :: Zipper a -> a
value = btValue . zipTree

left :: Zipper a -> Maybe (Zipper a)
left = child Left

right :: Zipper a -> Maybe (Zipper a)
right = child Right

child :: Direction -> Zipper a -> Maybe (Zipper a)
child direction (Zipper tree focus parents) =
  do c <- getChild focus
     return $ Zipper tree c (focus:parents)
       where getChild = case direction of Left -> btLeft
                                          Right -> btRight

up :: Zipper a -> Maybe (Zipper a)
up (Zipper tree _ (parent:rest)) = Just $ Zipper tree parent rest
up (Zipper _ _ []) = Nothing

setValue :: a -> Zipper a -> Zipper a
setValue _ = error "undefined"
-- setValue v (Zipper (BT _ l r) parent) = Zipper (BT v l r) parent

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft _ = error "undefined"
-- setLeft l (Zipper (BT v _ r) parent) = Zipper (BT v l r) parent

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight _ = error "undefined"
-- setRight r (Zipper (BT v l _) parent) = Zipper (BT v l r) parent

-- Debug
printBT :: Show a => BinTree a -> IO ()
printBT = putStr . unlines . prettyBT

prettyBT :: Show a => BinTree a -> [String]
prettyBT = go 0
  where go indent tree =
          [valueHeader, leftHeader] ++ leftTree ++ [rightHeader] ++ rightTree
          where indentString = replicate indent ' '
                valueHeader = indentString ++ "value: " ++ show (btValue tree)
                leftHeader = indentString ++ "left:"
                rightHeader = indentString ++ "right"
                leftTree = maybe defaultTree (go nextIndent) (btLeft tree)
                rightTree = maybe defaultTree (go nextIndent) (btRight tree)
                defaultTree = [indentString ++ "N/A"]
                nextIndent = indent + 1

leaf v     = node v Nothing Nothing                  
node v l r = Just (BT v l r :: BinTree Int)          
t1         = BT 1 (node 2 Nothing  $ leaf 3) $ leaf 4
t2         = BT 1 (node 5 Nothing  $ leaf 3) $ leaf 4
t3         = BT 1 (node 2 (leaf 5) $ leaf 3) $ leaf 4
t4         = BT 1 (leaf 2                  ) $ leaf 4
t5         = BT 6 (leaf 7                  ) $ leaf 8
t6         = BT 1 (node 2 Nothing  $ leaf 3) $ node 6 (leaf 7) (leaf 8)
t7         = BT 1 (node 2 Nothing  $ leaf 5) $ leaf 4
