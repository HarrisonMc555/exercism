module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node), rootLabel)
import Data.Maybe (listToMaybe, maybeToList, mapMaybe)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV = fromPovHelper []

fromPovHelper :: Eq a => [Tree a] -> a -> Tree a -> Maybe (Tree a)
fromPovHelper stack searchValue tree@(Node root children)
  | searchValue == root =
      let maybeNewChild = recreateFromStack root stack
      in Just $ addMaybeChild tree maybeNewChild
  | otherwise =
      let newStack = tree : stack
          newRoot = findMaybe (fromPovHelper newStack searchValue) children
      in flip addChild tree <$> newRoot

addChild :: Tree a -> Tree a -> Tree a
addChild (Node root children) child = Node root (child : children)

addMaybeChild :: Tree a -> Maybe (Tree a) -> Tree a
addMaybeChild tree Nothing = tree
addMaybeChild tree (Just child) = addChild tree child

recreateFromStack :: Eq a => a -> [Tree a] -> Maybe (Tree a)
recreateFromStack _ [] = Nothing
recreateFromStack toRemove (Node root children:rest) =
  let filteredChildren = filter ((/= toRemove) . rootLabel) children
      newChild = recreateFromStack root rest
      newChildren = maybeToList newChild ++ filteredChildren
  in Just (Node root newChildren)

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = error "You need to implement this function."

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = listToMaybe . mapMaybe f
