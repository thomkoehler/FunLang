
{-# LANGUAGE DeriveGeneric #-}

module TIM.Types where

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint
import qualified TIM.Heap as Heap
import qualified Data.Text as T
import qualified Data.Map as Map

import Utils.PrettyPrint

type Addr = Int
type Name = T.Text

data Instruction
   = Take {-# UNPACK #-} !Int
   | Enter !AMode
   | Push !AMode
   deriving(Show, Generic)

instance Out Instruction

data AMode
   = Arg {-# UNPACK #-} !Int
   | Label {-# UNPACK #-} !Name
   | Code ![Instruction]
   | IntConst {-# UNPACK #-} !Int
   deriving(Show, Generic)

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
   doc (TimState is fp st hp cs) = braces $ docIs <> docFp <> docSt <> docHp <> docCs
      where
         docIs = text "instructions: " <> doc is
         docFp = text ", frame: " <> doc fp
         docSt = text ", stack: " <> doc st
         docHp = text ", heap: " <> doc hp
         docCs = text ", code store: " <> doc cs


   docPrec _ = doc
