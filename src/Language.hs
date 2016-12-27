
---------------------------------------------------------------------------------------------------

module Language where

import Data.ByteString.Lazy(ByteString)


type Program = [ScDefn]


data ScDefn = ScDefn
   {
      scName :: !ByteString,
      scArgs :: ![ByteString],
      scExpr :: !Expr
   }
   deriving(Show, Eq)


data Expr
   = EVar !ByteString
   | ENum !Int
   | ELet [(ByteString, Expr)] Expr
   | EAp Expr Expr
   deriving(Show, Eq)

---------------------------------------------------------------------------------------------------
