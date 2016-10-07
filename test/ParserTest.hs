
---------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserTest where

import Test.Framework
import Text.RawString.QQ
import qualified Data.Text as T

import Compiler
import Language


prop_simpleMain :: Bool
prop_simpleMain = parse "SimpleMain" "main = 1" == Right [ScDefn "main" [] (ENum 1)]


strSimpleLet :: String
strSimpleLet = [r|
main =
   let
      x = 1
   in
      x
|]

exprSimpleLet :: Program
exprSimpleLet = 
   [
      ScDefn "main" [] 
      (
         ELet [("x", (ENum 1))] (EVar "x")
      )
   ]

prop_simpleLet :: Bool
prop_simpleLet = parse "SimpleLet" (T.pack strSimpleLet) == Right exprSimpleLet


strMultiLet :: String
strMultiLet = [r|
main =
   let
      x = 1
      y = 2
   in
      x
|]

exprMultiLet :: Program
exprMultiLet = 
   [
      ScDefn "main" [] 
      (
         ELet [("x", (ENum 1)), ("y", (ENum 2))] (EVar "x")
      )
   ]

prop_multiLet :: Bool
prop_multiLet = parse "MultiLet" (T.pack strMultiLet) == Right exprMultiLet


---------------------------------------------------------------------------------------------------