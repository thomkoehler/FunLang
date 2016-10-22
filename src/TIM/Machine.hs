
module TIM.Machine where

import TIM.Types
import qualified TIM.Heap as Heap

data TimState = TimState
   {
      instructions :: [Instruction],
      framePtr :: Address,
      stack :: [Address],
      heap :: Heap.Heap Address Closure,
      codeStore :: CodeStore
   }
   
   
take :: Int -> TimState -> TimState
take n state = state
   {
      instructions = tail $ instructions state,
      stack = drop n $ stack state
   }

