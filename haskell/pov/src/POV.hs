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
tracePathBetween _ _ _ = error "You need to implement this function."

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = listToMaybe . mapMaybe f

fancyShow :: Show a => Tree a -> String
fancyShow = unlines . fancyShowHelper 0
  where fancyShowHelper level (Node label children) =
          let thisLine = repeatN level " | " ++ show label
          in thisLine : concatMap (fancyShowHelper (level + 1)) children
        repeatN n x = concat . take n $ repeat x
          

leaf v = Node v []

simple = Node "parent" [ leaf "x"
                       , leaf "sibling"
                       ]

fancyPrint :: (Show a) => Tree a -> IO ()
fancyPrint = putStrLn . fancyShow
