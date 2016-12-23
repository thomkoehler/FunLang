
---------------------------------------------------------------------------------------------------

module Compiler.Lexer
(
   Compiler.Lexer.identifier,
   Compiler.Lexer.natural,
   Compiler.Lexer.reserved,
   Compiler.Lexer.reservedOp,
   Compiler.Lexer.parens
)
where

import Text.Parsec hiding (State)
import Control.Monad.State
import Text.Parsec.Token
import Data.Text
import qualified Text.Parsec.Token as P


import Compiler.Types

languageDef :: GenLanguageDef Text st (State SourcePos)
languageDef = P.LanguageDef
   {
      P.commentStart = "/*",
      P.commentEnd = "*/",
      P.commentLine  = "//",
      P.nestedComments = True,
      P.identStart  = letter,
      P.identLetter = alphaNum <|> oneOf "_'",
      P.reservedNames =
         [
            "let",
            "in",
            "Pack",
            "case",
            "of",
            "->"
         ],
      P.opStart = P.opLetter languageDef,
      P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      P.reservedOpNames = ["+", "-", "*", "/", "<", ">"],
      P.caseSensitive  = True
   }


lexer :: P.GenTokenParser Text () (State SourcePos)
lexer = P.makeTokenParser languageDef

identifier :: IParser Text
identifier = pack <$> P.identifier lexer

natural :: IParser Int
natural = fromEnum <$> P.natural lexer

parens :: IParser a -> IParser a
parens = P.parens lexer

reserved :: String -> IParser ()
reserved = P.reserved lexer

reservedOp :: String -> IParser ()
reservedOp = P.reservedOp lexer

---------------------------------------------------------------------------------------------------
