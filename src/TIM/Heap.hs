---------------------------------------------------------------------------------------------------

module TIM.Heap
(
   Heap,
   empty,
   insert,
   update,
   free
)
where

import qualified Data.Map.Strict as Map


data Heap a d = Heap (Map.Map a d) a

empty :: (Num a, Ord a) => Heap a d
empty = Heap Map.empty 1

insert :: (Num a, Ord a) => d -> Heap a d -> (Heap a d, a)
insert x (Heap heapMap newAddr) =
   (Heap (Map.insert newAddr x heapMap) (newAddr + 1), newAddr)


update :: (Num a, Ord a) => a -> d -> Heap a d -> Heap a d
update addr x (Heap heapMap newAddr) = Heap (Map.insert addr x heapMap) newAddr


free :: (Num a, Ord a) => a -> Heap a d -> Heap a d
free addr (Heap heapMap newAddr) = Heap (Map.delete addr heapMap) newAddr

---------------------------------------------------------------------------------------------------
