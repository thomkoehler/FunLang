
module TIM.Machine where

import qualified TIM.Heap as Heap
import qualified Data.Text as Text
import qualified Data.Map as Map


type Addr = Int

data Instruction 
   = Take !Int
   | Enter !AMode
   | Push !AMode


data AMode
   = Arg !Int
   | Label !Text.Text
   | Code ![Instruction]
   | IntConst !Int


data FramePtr 
   = FrameAddr Addr
   | FrameInt Int
   | FrameNull


type Closure = ([Instruction], FramePtr)
type TimHeap = Heap.Heap Addr Closure 
type TimStack = [Closure] 
type CodeStore = Map.Map Text.Text [Instruction]

data TimState = TimState
   {
      instructions :: [Instruction],
      framePtr :: FramePtr,
      stack :: TimStack,
      heap :: TimHeap,
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
         

