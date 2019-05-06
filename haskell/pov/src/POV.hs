module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node), rootLabel)
import Data.Maybe (listToMaybe, maybeToList, mapMaybe)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV = fromPovHelper []

fromPovHelper :: Eq a => [Tree a] -> a -> Tree a -> Maybe (Tree a)
fromPovHelper stack searchValue tree@(Node root children)
  | root == searchValue =
      let maybeNewChild = recreateFromStack root stack
      in Just $ addMaybeChild tree maybeNewChild
  | otherwise =
      let newStack = tree : stack
      in findMaybe (fromPovHelper newStack searchValue) children

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
tracePathBetween start end tree = reverse <$> maybeBackwardsPath
  where maybeBackwardsPath = newTree >>= tracePathBetweenHelper [] end
        newTree = fromPOV start tree
        tracePathBetweenHelper :: Eq a => [a] -> a -> Tree a -> Maybe [a]
        tracePathBetweenHelper path searchValue (Node root children)
          | root == searchValue = Just newPath
          | otherwise = findMaybe (tracePathBetweenHelper newPath searchValue) children
            where newPath = root : path

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = listToMaybe . mapMaybe f
