module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)


data Node a = Node (NodeRef a) a (NodeRef a)
            | Nil
type NodeRef a = IORef (Node a)
data Deque a = Deque (IORef (NodeRef a)) (IORef (NodeRef a))

mkDeque :: IO (Deque a)
mkDeque = do
  h <- mkEmptyNodeRef
  t <- mkEmptyNodeRef
  href <- newIORef h
  tref <- newIORef t
  return (Deque href tref)

mkEmptyNodeRef :: IO (NodeRef a)
mkEmptyNodeRef = newIORef Nil

mkNodeRef :: a -> IO (NodeRef a)
mkNodeRef a = do
  p <- mkEmptyNodeRef
  n <- mkEmptyNodeRef
  newIORef (Node p a n)

push :: Deque a -> a -> IO ()
push (Deque href tref) v = do
  newNode <- mkNodeRef v
  (Node newP _ _) <- readIORef newNode
  h <- readIORef href
  hval <- readIORef h
  writeIORef href newNode
  case hval of
    Nil ->
      writeIORef tref newNode
    Node _ headV headN ->
      writeIORef h (Node newNode headV headN) >>
      writeIORef newNode (Node newP v h)

pop :: Deque a -> IO (Maybe a)
pop (Deque href tref) = do
  h <- readIORef href
  hval <- readIORef h
  case hval of
    (Node pref hv nref) -> do
      writeIORef href nref
      n <- readIORef nref
      case n of
        (Node _ nv nnref) -> writeIORef nref (Node pref nv nnref)
        Nil -> writeIORef tref nref
      return (Just hv)
    Nil -> return Nothing

unshift :: Deque a -> a -> IO ()
unshift (Deque href tref) v = do
  newNode <- mkNodeRef v
  (Node _ _ newN) <- readIORef newNode
  t <- readIORef tref
  tval <- readIORef t
  writeIORef tref newNode
  case tval of
    Nil ->
      writeIORef href newNode
    Node tailP tailV _ ->
      writeIORef t (Node tailP tailV newNode) >>
      writeIORef newNode (Node t v newN)

shift :: Deque a -> IO (Maybe a)
shift (Deque href tref) = do
  t <- readIORef tref
  tval <- readIORef t
  case tval of
    (Node pref tv nref) -> do
      writeIORef tref pref
      p <- readIORef pref
      case p of
        (Node ppref pv _) -> writeIORef pref (Node ppref pv nref)
        Nil -> writeIORef href pref
      return (Just tv)
    Nil -> return Nothing
