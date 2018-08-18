{-# LANGUAGE InstanceSigs, MultiWayIf #-}
module Set where

class Set s where
  empty :: Ord a => s a -> Bool
  insert :: Ord a => a -> s a -> s a
  member :: Ord a => a -> s a -> Bool

data MyTree a = Empty | Node (MyTree a) a (MyTree a) deriving (Show, Eq, Ord)

instance Set MyTree where
  empty :: Ord a => MyTree a -> Bool
  empty t = case t of
    Empty -> True
    _ -> False
  insert :: Ord a => a -> MyTree a -> MyTree a
  insert v t = case t of
                 Empty -> Node Empty v Empty
                 Node t1 x t2 -> if | v < x -> Node (insert v t1) x t2
                                    | v > x -> Node t1 x (insert v t2)
                                    | otherwise -> t
  member :: Ord a => a -> MyTree a -> Bool
  member v t = case t of
    Empty -> False
    Node t1 x t2 -> if | v == x -> True
                       | v < x -> member v t1
                       | v > x -> member v t2
