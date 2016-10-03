---------------------------------------------------------------------------------------------------

module Main where

import qualified Data.Text.IO as TIO

import Parser

---------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   content <- TIO.readFile "testSrc/simpleMain.fl"
   let prg = iParse "testSrc/simpleMain.fl" content
   print prg

---------------------------------------------------------------------------------------------------
