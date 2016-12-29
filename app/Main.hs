---------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty
import qualified Data.ByteString.Lazy as ByteString

import Compiler
import TIM

---------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   content <- ByteString.readFile "testSrc/prelude.fl"
   let ast = parse content
   putStrLn $ render . doc $ ast
   putStrLn $ render . doc . eval . compile . compileProgram $ ast

---------------------------------------------------------------------------------------------------
