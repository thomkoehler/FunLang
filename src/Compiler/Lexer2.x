{
module Compiler.Lexer2() where

data Token
   = TTrue
   | TFalse
   | TZero
   | TSucc
   | TPred
   | TIf
   | TThen
   | TElse
   | TIsZero
   | TEOF
   deriving (Eq,Show)

}

%wrapper "basic"

$letter = [a-zA-Z]
$nonletter = [~$letter\n]

tokens :-
  $nonletter+;
  $letter+ {id}
