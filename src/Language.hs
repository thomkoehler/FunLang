
---------------------------------------------------------------------------------------------------

module Language where

import Data.Text


type Program = [ScDefn]


data ScDefn = ScDefn
   {
      scName :: !Text,
      scArgs :: ![Text],
      scExpr :: !Expr
   }
   deriving(Show, Eq)


data Expr
   = EVar !Text
   | ENum !Int
   | ELet [(Text, Expr)] Expr
   | EAp Expr Expr
   deriving(Show, Eq)

---------------------------------------------------------------------------------------------------
