---------------------------------------------------------------------------------------------------

module Compiler.Types(IParser) where

import Text.Parsec hiding (State)
import Control.Monad.State
import qualified Data.Text as T


type IParser a = ParsecT T.Text () (State SourcePos) a

---------------------------------------------------------------------------------------------------
