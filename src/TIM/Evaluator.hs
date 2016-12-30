
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  QuasiQuotes #-}

module TIM.Evaluator(compile, eval) where


import TIM.Types
import qualified TIM.Heap as Heap
import TIM.FunQuoter

import Data.Maybe
import Text.Printf
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map as Map


compile :: CodeStore -> TimState
compile program = TimState
   {
      instructions = [Enter (Label "main")],
      framePtr = FrameNull,
      stack = [],
      valueStack = [],
      heap = Heap.empty,
      codeStore = Map.union preludeDefs program
   }


preludeDefs :: CodeStore
preludeDefs = Map.fromList
   [
      ("+", compilePrimitives "+"),
      ("*", compilePrimitives "*")
   ]

{- TODO
preludeDefs = [fun|
   I x = x ;
   K x y = x ;
   K1 x y = y ;
   S f g x = f (g x) ;
|]
-}

compilePrimitives :: ByteString.ByteString -> [Instruction]
compilePrimitives op =
   [
      Take 2,
      Push
      (
         Code
         [
            Push (Code [Op (strToOp op), Return]),
            Enter (Arg 1)
         ]
      )
   ]
   where
      strToOp "*" = Mult
      strToOp "+" = Add


lookupCodeStore :: Name -> CodeStore -> [Instruction]
lookupCodeStore name cs = fromMaybe (error (printf "cs: Not in scope: '%s'" (C.unpack name))) $ Map.lookup name cs


--TODO intCode
intCode :: Int -> [Instruction]
intCode _ = [PushV FramePtr, Return]


-- | take n closures from stack and create a new frame
takeN :: Int      -- ^ size of closures
   -> TimState    -- ^ state before
   -> TimState    -- ^ state after

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


-- | create closure from argument and push it onto stack
pushArg :: AMode  -- ^ argument
   -> TimState    -- ^ state before
   -> TimState    -- ^ state after

pushArg am state@(TimState _ fp st vst h cs) = state
   {
      stack = amodeToClosure am fp h cs : st
   }


-- | create closure from argument
amodeToClosure :: AMode    -- ^ argument
   -> FramePtr             -- ^ current frame
   -> TimHeap              -- ^ heap
   -> CodeStore            -- ^ code store
   -> Closure              -- ^ new closure

amodeToClosure (Label l) fp _ cs = (lookupCodeStore l cs, fp)

amodeToClosure (Arg k) (FrameAddr addr) hp _ =
   let
      frame = fromMaybe (error (printf "Heap read error address %d" addr)) $ Heap.get addr hp
   in
      frame !! (k -1)

amodeToClosure (Code instrs) fp _ _ = (instrs, fp)

amodeToClosure (IntConst n) _ _ _ = (intCode n, FrameInt n)


-- |  get closure at position from frame
createClosure :: Int -- ^ position
   -> FramePtr       -- ^ frame
   -> TimHeap        -- ^ heap
   -> Closure        -- ^ closure
createClosure n (FrameAddr addr) hp
   = fromMaybe (error (printf "Read from wrong address: %d" addr)) (Heap.get addr hp) !! n

createClosure _ (FrameInt _) _ = undefined
createClosure _ FrameNull _ = error "Read from NULL address."


eval :: TimState -> [TimState]
eval state = state : restState
   where
      restState | timFinal state = []
                | otherwise = eval nextState

      nextState = step state


timFinal :: TimState -> Bool
timFinal (TimState [] _ _ _ _ _) = True
timFinal _ = False


-- | take the next step and call it
step :: TimState  -- ^ state before
   -> TimState    -- ^state after

step state@(TimState (Take n : instr) _ st _ hp _)
   | length st >= n = state
      {
         instructions = instr,
         framePtr = FrameAddr addr,
         stack = drop n st,
         heap = hp'
      }
   | otherwise = error "Stack overflow."
   where
      (hp', addr) = Heap.insert (take n st) hp



step state@(TimState [Enter am] fptr _ _ hp cs) = state
   {
      instructions = instructions',
      framePtr = framePtr'
   }
   where
      (instructions', framePtr') = amodeToClosure am fptr hp cs


step state@(TimState (Push am : instr) fptr st _ hp cs) = state
   {
      instructions = instr,
      stack = amodeToClosure am fptr hp cs : st
   }


step state@(TimState [Return] _ ((instr', framePtr'):st') _ _ _) = state
   {
      instructions = instr',
      stack = st',
      framePtr = framePtr'
   }

step state@(TimState (PushV FramePtr : instr) (FrameInt n) _ vst _ _) = state
   {
      instructions = instr,
      valueStack = n : vst
   }

step (TimState is _ _ _ _ _) = error $ "TimState not supported: " ++ show is
