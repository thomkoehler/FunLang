
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.PrettyPrint where

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map.Strict
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint


instance Out ByteString.ByteString where
   doc = text . C.unpack
   docPrec _ = doc


instance (Out n, Out i) => Out (Data.Map.Strict.Map n i) where
   doc = braces . docList . Data.Map.Strict.toList
   docPrec _ = doc
