
---------------------------------------------------------------------------------------------------

module Language where

import Data.Text


type Program a = [ScDefn a]

data ScDefn a = ScDefn
   {
      scName :: !a,
      scArgs :: ![a],
      scExpr :: !(Expr a)
   }
   deriving(Show, Eq)

data Expr a
   = EVar !a
   | ENum !Integer
   | ELet [(a, Expr a)] (Expr a)
   deriving(Show, Eq)

---------------------------------------------------------------------------------------------------
