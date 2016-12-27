---------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

import Compiler
import TIM

---------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   let content = "main = S K K 4;"
   putStrLn $ render . doc . eval . compile . compileProgram . parse $ content

---------------------------------------------------------------------------------------------------
