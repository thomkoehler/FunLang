---------------------------------------------------------------------------------------------------

module Compiler.Parser(Compiler.Parser.parse) where

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State
import qualified Data.Text as T


import Language
import Compiler.Types
import Compiler.Lexer


parse :: SourceName -> T.Text -> Either ParseError Program
parse sourceName input = runIndent sourceName $ runParserT parseProgram () sourceName input


parseProgram :: IParser Program
parseProgram = many parseScDefn


parseScDefn :: IParser ScDefn
parseScDefn = do
   spaces
   name <- identifier
   args <- many identifier
   reservedOp "="
   expr <- parseExpr
   return $ ScDefn name args expr


parseExpr :: IParser Expr
parseExpr = do
   spaces
   choice
      [
         parseLetExpr,
         EVar <$> identifier,
         ENum <$> natural
      ]
   <?> "expr"


parseLetExpr :: IParser Expr
parseLetExpr = do
   reserved "let"
   as <- block parseAlias
   reserved "in"
   expr <- parseExpr
   return $ ELet as expr


parseAlias :: IParser (T.Text, Expr)
parseAlias = do
   alias <- identifier
   reservedOp "="
   expr <- parseExpr
   return (alias, expr)


---------------------------------------------------------------------------------------------------
