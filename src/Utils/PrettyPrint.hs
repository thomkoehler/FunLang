
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.PrettyPrint where

import qualified Data.Text
import qualified Data.Map.Strict
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint


instance Out Data.Text.Text where
   doc = text . Data.Text.unpack
   docPrec _ = doc


instance (Out n, Out i) => Out (Data.Map.Strict.Map n i) where
   doc = braces . docList . Data.Map.Strict.toList
   docPrec _ = doc
