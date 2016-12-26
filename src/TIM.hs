
module TIM
(
   module TIM.Evaluator,
   module TIM.Types,
   module TIM.Compiler
)
where

import TIM.Evaluator(compile, eval)
import TIM.Types
import TIM.Compiler(compileProgram)
