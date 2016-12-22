
module TIM.Evaluator where


import TIM.Types
import qualified TIM.Heap as Heap

import Data.Maybe
import Text.Printf
import qualified Data.Map as Map


lookupCodeStore :: Name -> CodeStore -> [Instruction]
lookupCodeStore name cs = fromMaybe (error (printf "Not in scope: '%s'" name)) $ Map.lookup name cs


--TODO intCode
intCode :: Integer -> [Instruction]
intCode _ = []


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

pushArg am state@(TimState _ fp st h cs) = state
   {
      stack = amodeToClosure am fp h cs : st
   }


-- | create closure from argument
amodeToClosure :: AMode    -- ^ argument
   -> FramePtr             -- ^ current frame
   -> TimHeap              -- ^ heap
   -> CodeStore            -- ^ code store
   -> Closure              -- ^ new closure

amodeToClosure (Arg k) fp hp _ = createClosure k fp hp
amodeToClosure (Code is) fp _ _ = (is, fp)
amodeToClosure (Label name) fp _ cs = (lookupCodeStore name cs, fp)
amodeToClosure (IntConst iConst) fp _ _ = (intCode iConst, fp)


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
timFinal (TimState [] _ _ _ _) = True
timFinal _ = False


-- | take next step and call it
step :: TimState  -- ^ state before
   -> TimState    -- ^state after

step state@(TimState (Take n : instr) _ st hp _)
   | length st >= n = state
      {
         instructions = instr,
         framePtr = FrameAddr addr,
         stack = drop n st,
         heap = hp'
      }
      where
         (hp', addr) = Heap.insert (take n st) hp


step state@(TimState [Enter am] fptr st hp cs) = state
   {
      instructions = instr,
      framePtr = FrameAddr addr
   }
   where
      (instr, addr) = amodeToClosure am fptr hp cs


step state@(TimState (Push am : instr) fptr st hp cs) = state
   {
      instructions = instr,
      stack = amodeToClosure am fptr hp cs : st
   }
