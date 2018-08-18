{-# LANGUAGE RankNTypes, InstanceSigs, MultiParamTypeClasses #-}
module List where

import Prelude hiding (head, tail)

class List l where
  empty :: l a -> Bool
  cons ::  a -> l a -> l a
  head ::  l a -> a
  tail ::  l a -> l a

data MyList a = Empty | Cons a (MyList a) deriving (Show)

instance List MyList where
  empty :: MyList a -> Bool
  empty Empty = True
  empty _ = False
  head :: MyList a -> a
  head (Cons a _) = a
  head Empty = error "empty"
  tail :: MyList a -> MyList a
  tail (Cons _ as) = as
  tail Empty = error "empty"
  cons :: a -> MyList a -> MyList a
  cons = Cons

conc :: MyList a -> MyList a -> MyList a
conc Empty bs = bs
conc (Cons a as) bs = Cons a (conc as bs)

update :: MyList a -> Int -> a -> MyList a
update as i a = case i of
  0 -> Cons a (tail as)
  _ -> Cons (head as) (update (tail as) (i - 1) a)

suffixes :: MyList a -> MyList (MyList a)
suffixes Empty = Empty
suffixes ass@(Cons _ as) = Cons ass (suffixes as)
