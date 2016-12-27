
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TIM.Types where

import Data.Data
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint
import qualified TIM.Heap as Heap
import qualified Data.Map as Map
import Data.ByteString.Lazy(ByteString)

import Utils.PrettyPrint()

type Addr = Int
type Name = ByteString

data Instruction
   = Take {-# UNPACK #-} !Int
   | Enter !AMode
   | Push !AMode
   deriving(Show, Generic, Data)

instance Out Instruction

data AMode
   = Arg {-# UNPACK #-} !Int
   | Label {-# UNPACK #-} !Name
   | Code ![Instruction]
   | IntConst {-# UNPACK #-} !Int
   deriving(Show, Generic, Data)

instance Out AMode

data FramePtr
   = FrameAddr {-# UNPACK #-} !Addr
   | FrameInt {-# UNPACK #-} !Int
   | FrameNull
   deriving(Show, Generic)

instance Out FramePtr


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
   deriving(Show, Generic)

instance Out TimState where
   doc (TimState is fp st hp cs) = vcat
      [
         text "TimState:",
         nest 1 (text "instructions: " <> doc is),
         nest 1 (text "frame: " <> doc fp),
         nest 1 (text "stack: " <> doc st),
         nest 1 (text "heap: " <> doc hp),
         nest 1 (text "code store: " <> doc cs)
      ]

   docPrec _ = doc
