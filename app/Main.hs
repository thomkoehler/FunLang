---------------------------------------------------------------------------------------------------

module Main where

import qualified Data.Text.IO as TIO

import Parser

---------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   let file = "testSrc/nestedLet.fl"
   content <- TIO.readFile file
   let prg = parse file content
   print prg

---------------------------------------------------------------------------------------------------
