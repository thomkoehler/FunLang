
---------------------------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language where

import Data.Data
import Data.ByteString.Lazy(ByteString)
import Text.PrettyPrint.GenericPretty

import Utils.PrettyPrint()


type Program = [ScDefn]


data ScDefn = ScDefn
   {
      scName :: !ByteString,
      scArgs :: ![ByteString],
      scExpr :: !Expr
   }
   deriving(Show, Eq, Data, Generic)

instance Out ScDefn

data Expr
   = EVar !ByteString
   | ENum !Int
   | ELet [(ByteString, Expr)] Expr
   | EAp Expr Expr
   deriving(Show, Eq, Data, Generic)

instance Out Expr

---------------------------------------------------------------------------------------------------
