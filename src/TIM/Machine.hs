
module TIM.Machine where

import TIM.Types
import qualified TIM.Heap as Heap

data Closure = Closure

data Frame = Frame
   {
     closures :: [Closure] 
   }



data TimState = TimState
   {
      instructions :: [Instruction],
      framePtr :: Address,
      stack :: [Closure],
      heap :: Heap.Heap Address Frame,
      codeStore :: CodeStore
   }
   
   
take :: Int -> TimState -> TimState
take n state = 
   let
      frame = Frame $ Prelude.take n $ stack state
      (newHeap, newFramePtr) = Heap.insert frame $ heap state
   in
      state
         {
            instructions = tail $ instructions state,
            framePtr = newFramePtr,
            stack = drop n $ stack state,
            heap = newHeap
         }
         

