
{

{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler.Tokens(Token(..), scanTokens) where

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as C

}

%wrapper "posn-bytestring"


$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  \=                            { \p _ -> TkCharSym p '=' }
  \+                            { \p _ -> TkCharSym p '+' }
  \-                            { \p _ -> TkCharSym p '-' }
  \*                            { \p _ -> TkCharSym p '*' }
  \/                            { \p _ -> TkCharSym p '/' }
  \(                            { \p _ -> TkCharSym p '(' }
  \)                            { \p _ -> TkCharSym p ')' }
  \{                            { \p _ -> TkCharSym p '{' }
  \}                            { \p _ -> TkCharSym p '}' }
  \;                            { \p _ -> TkCharSym p ';' }
  \\                            { \p _ -> TkCharSym p '\\' }
  \.                            { \p _ -> TkCharSym p '.' }
  \<                            { \p _ -> TkCharSym p '<' }
  >                             { \p _ -> TkCharSym p '>' }
  let                           { \p _ -> TkStrSym p "let" }
  letrec                        { \p _ -> TkStrSym p "letrec" }
  in                            { \p _ -> TkStrSym p "in" }
  case                          { \p _ -> TkStrSym p "case" }
  of                            { \p _ -> TkStrSym p "of" }
  "->"                          { \p _ -> TkStrSym p "->" }
  "neg"                         { \p _ -> TkStrSym p "neg" }
  $alpha [$alpha $digit \_ \']* { \p s -> TokenId p s }
  $digit+                       { \p s -> TokenInt p (read (C.unpack s)) }

{

tok f p s = f p s

data Token
   = TokenInt !AlexPosn !Int
   | TokenId !AlexPosn !ByteString.ByteString
   | TkCharSym !AlexPosn !Char
   | TkStrSym !AlexPosn !ByteString.ByteString
   deriving (Eq, Show)

scanTokens = alexScanTokens

}
