
module TIM.FunQuoter where

import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH as TH
import qualified Data.ByteString.Lazy.Char8 as C

import Compiler
import TIM.Compiler

fun :: QuasiQuoter
fun = QuasiQuoter
   {
      quoteExp = quoteExpFunTerm,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
   }


quoteExpFunTerm :: String -> TH.Q TH.Exp
quoteExpFunTerm = dataToExpQ (const Nothing) . compileProgram . parse . C.pack
