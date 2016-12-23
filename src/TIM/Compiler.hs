
module TIM.Compiler where

import Language

import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Maybe
import Text.Printf

import TIM.Types


type TimCompilerEnv = Map.Map Text.Text AMode


compileProgram :: TimCompilerEnv -> Program -> CodeStore
compileProgram env prog = Map.fromList $ map (compileScDefn env) prog


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
compileToAMode (EVar v) env = fromMaybe (error (printf "Not in scope: '%s'" v)) $ Map.lookup v env
compileToAMode (ENum n) _ = IntConst n
compileToAMode e env = Code $ compileToInstructions e env
