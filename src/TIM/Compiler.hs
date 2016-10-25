
module TIM.Compiler where

import Language

import qualified Data.Map as Map
import qualified Data.Text as Text

import TIM.Types


type TimCompilerEnv = Map.Map Text.Text AMode

compile :: Program -> CodeStore
compile = undefined


compilerScDefn = undefined