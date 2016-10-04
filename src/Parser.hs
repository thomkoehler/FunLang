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
parseProgram = many parseScDefn


parseScDefn :: IParser (ScDefn T.Text)
parseScDefn = do
   spaces
   name <- identifier
   args <- many identifier
   reservedOp "="
   expr <- parseExpr
   return $ ScDefn name args expr


parseExpr :: IParser (Expr T.Text)
parseExpr = do
   spaces
   choice
      [
         parseLetExpr,
         EVar <$> identifier,
         ENum <$> natural
      ]
   <?> "expr"


parseLetExpr :: IParser (Expr T.Text)
parseLetExpr = do
   as <- withBlock' (reserved "let") parseAlias
   reserved "in"
   expr <- parseExpr
   return $ ELet as expr


parseAlias :: IParser (T.Text, Expr T.Text)
parseAlias = do
   alias <- identifier
   reservedOp "="
   expr <- parseExpr
   return (alias, expr)


---------------------------------------------------------------------------------------------------
