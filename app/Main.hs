---------------------------------------------------------------------------------------------------

module Main where

import qualified Data.Text.IO as TIO
import Text.PrettyPrint

import Compiler
import TIM

---------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   let file = "testSrc/nestedLet.fl"
   content <- TIO.readFile file
   case parse file content of
      Left err -> print err
      Right prg -> print $ render . eval . compile $ prg

---------------------------------------------------------------------------------------------------
