{-# LANGUAGE FlexibleContexts, InstanceSigs #-}
module Heap where

class Heap h where
  empty :: Ord a => h a
  isEmpty :: Ord a => h a -> Bool
  insert :: Ord a => a -> h a -> h a
  merge :: Ord a => h a -> h a -> h a
  findMin :: Ord a => h a -> a
  deleteMin :: Ord a => h a -> h a

data LeftistTree a = Empty | Node Integer a (LeftistTree a) (LeftistTree a)

rank :: LeftistTree a -> Integer
rank Empty = 0
rank (Node r _ _ _) = r

makeTree :: Ord a => a -> LeftistTree a -> LeftistTree a -> LeftistTree a
makeTree x at bt
  | ar < br = Node (br + 1) x at bt
  | otherwise = Node (ar + 1) x at bt
  where
    ar = rank at
    br = rank bt


instance Heap LeftistTree where
  empty :: Ord a => LeftistTree a
  empty = Empty :: LeftistTree a

  isEmpty :: Ord a => LeftistTree a -> Bool
  isEmpty h = case h of
    Empty -> True
    _ -> False

  insert :: Ord a => a -> LeftistTree a -> LeftistTree a
  insert x = merge (Node 0 x Empty Empty)

  merge :: Ord a => LeftistTree a -> LeftistTree a -> LeftistTree a
  merge t Empty = t
  merge Empty t = t
  merge t1@(Node ar av a1 a2) t2@(Node br bv b1 b2)
    | av <= bv = makeTree av a1 (merge a2 t2)
    | otherwise = makeTree bv b1 (merge t1 b2)

  findMin :: Ord a => LeftistTree a -> a
  findMin Empty = error "empty heap"
  findMin (Node _ x _ _) = x

  deleteMin :: Ord a => LeftistTree a -> LeftistTree a
  deleteMin Empty = error "empty heap"
  deleteMin (Node _ _ a b) = merge a b
