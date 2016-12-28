
module TIM.Compiler(compileProgram) where

import Language

import qualified Data.Map as Map
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe
import Text.Printf

import TIM.Types


type TimCompilerEnv = Map.Map Name AMode


compileProgram :: Program -> CodeStore
compileProgram prog = Map.fromList $ map (compileScDefn Map.empty) prog


compileScDefn :: TimCompilerEnv -> ScDefn -> (Name, [Instruction])
compileScDefn env (ScDefn name args body) = (name, Take (length args) : instrs)
   where
      newEnv = Map.union env $ Map.fromList $ zip args $ map Arg [1..]
      instrs = compileToInstructions body newEnv


compileToInstructions :: Expr -> TimCompilerEnv -> [Instruction]
compileToInstructions (EAp e0 e1) env = Push (compileToAMode e1 env) : compileToInstructions e0 env
compileToInstructions ev@(EVar _) env = [Enter (compileToAMode ev env)]
compileToInstructions en@(ENum _) env = [Enter (compileToAMode en env)]
compileToInstructions _ _ = error "compileToInstructions: can't do this yet"


compileToAMode :: Expr -> TimCompilerEnv -> AMode
compileToAMode (EVar v) env = fromMaybe (Label v) $ Map.lookup v env
compileToAMode (ENum n) _ = IntConst n
compileToAMode e env = Code $ compileToInstructions e env
