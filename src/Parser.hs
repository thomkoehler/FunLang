---------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Parser(iParse) where

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State
import qualified Data.Text as T

import Types
import Language
import Lexer


iParse :: SourceName -> T.Text -> Either ParseError (Program T.Text)
iParse sourceName input = runIndent sourceName $ runParserT parseProgram () sourceName input

parseProgram :: IParser (Program T.Text)
parseProgram = do
   spaces
   many parseScDefn

parseScDefn :: IParser (ScDefn T.Text)
parseScDefn = do
   (name : args) <- many identifier
   spaces
   return $ ScDefn name [] (EVar "Undefined")

---------------------------------------------------------------------------------------------------
