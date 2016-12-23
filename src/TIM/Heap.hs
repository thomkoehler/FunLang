---------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module TIM.Heap
(
   Heap,
   empty,
   insert,
   update,
   free,
   get
)
where

import qualified Data.Map.Strict as Map
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint hiding(empty)


data Heap a d = Heap !(Map.Map a d) !a
   deriving(Show)

instance (Out n, Out i) => Out (Map.Map n i) where
   doc = braces . docList . Map.toList
   docPrec _ = doc


empty :: (Num a) => Heap a d
empty = Heap Map.empty 1

insert :: (Num a, Ord a) => d -> Heap a d -> (Heap a d, a)
insert x (Heap heapMap newAddr) =
   (Heap (Map.insert newAddr x heapMap) (newAddr + 1), newAddr)


update :: (Ord a) => a -> d -> Heap a d -> Heap a d
update addr x (Heap heapMap newAddr) = Heap (Map.insert addr x heapMap) newAddr


free :: (Ord a) => a -> Heap a d -> Heap a d
free addr (Heap heapMap newAddr) = Heap (Map.delete addr heapMap) newAddr

get :: Ord a => a -> Heap a d -> Maybe d
get addr (Heap m _) = Map.lookup addr m

---------------------------------------------------------------------------------------------------
