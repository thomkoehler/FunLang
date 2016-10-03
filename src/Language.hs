
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
   deriving(Show)

data Expr a
   = EVar !a
   | ENum !Int
   | ELet [(a, Expr a)] (Expr a)
   deriving(Show)

---------------------------------------------------------------------------------------------------
