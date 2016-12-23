
module TIM.Types where

import qualified TIM.Heap as Heap
import qualified Data.Text as T
import qualified Data.Map as Map

type Addr = Int
type Name = T.Text

data Instruction
   = Take {-# UNPACK #-} !Int
   | Enter !AMode
   | Push !AMode
   deriving(Show)


data AMode
   = Arg {-# UNPACK #-} !Int
   | Label {-# UNPACK #-} !Name
   | Code ![Instruction]
   | IntConst {-# UNPACK #-} !Int
   deriving(Show)


data FramePtr
   = FrameAddr {-# UNPACK #-} !Addr
   | FrameInt {-# UNPACK #-} !Int
   | FrameNull
   deriving(Show)


type Frame = [Closure]
type Closure = ([Instruction], FramePtr)
type TimHeap = Heap.Heap Addr Frame
type TimStack = [Closure]
type CodeStore = Map.Map Name [Instruction]

data TimState = TimState
   {
      instructions :: ![Instruction],
      framePtr :: !FramePtr,
      stack :: !TimStack,
      heap :: !TimHeap,
      codeStore :: !CodeStore
   }
   deriving(Show)
