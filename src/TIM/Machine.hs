
module TIM.Machine where

import qualified TIM.Heap as Heap
import qualified Data.Text as Text
import qualified Data.Map as Map

import Data.Maybe
import Text.Printf

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


type Frame = [Closure]
type Closure = ([Instruction], FramePtr)
type TimHeap = Heap.Heap Addr Frame
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
   
   
takeN :: Int -> TimState -> TimState
takeN n state = state
   {
      instructions = tail $ instructions state,
      framePtr = FrameAddr closuresAddr,
      stack = drop n $ stack state,
      heap = newHeap
   }
   where
      closures = take n (stack state)
      (newHeap, closuresAddr) = Heap.insert closures $ heap state
       

pushArg :: AMode -> TimState -> TimState
pushArg am state@(TimState _ fp st h cs) = state
   {
      stack = (amodeToClosure am fp h cs) : st
   }


amodeToClosure :: AMode -> FramePtr -> TimHeap -> CodeStore -> Closure
amodeToClosure (Arg k) fp hp _ = getClosureFromHeap k fp hp


getClosureFromHeap :: Int -> FramePtr -> TimHeap -> Closure
getClosureFromHeap k (FrameAddr addr) hp = (fromMaybe (error (printf "Read from wrong address: %d" addr)) (Heap.get addr hp)) !! k
getClosureFromHeap _ FrameNull _ = error "Read from NULL address."