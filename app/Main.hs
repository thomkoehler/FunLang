---------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

--import qualified Data.Text.IO as TIO
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

import Compiler
import TIM

---------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   let file = "testSrc/nestedLet.fl"
   --content <- TIO.readFile file
   let content = "main = 1"
   case parse file content of
      Left err -> print err
      Right prg -> print $ render . doc . eval . compile . compileProgram $ prg

---------------------------------------------------------------------------------------------------
