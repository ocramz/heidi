{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Encode
-- Description :  Generic encoding of algebraic datatypes
-- Copyright   :  (c) Marco Zocca (2019)
-- License     :  MIT
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Generic encoding of algebraic datatypes, using 'generics-sop'
--
-----------------------------------------------------------------------------
module Data.Generics.Encode (HasGE, OneHot, onehotDim, onehotIx) where

-- import qualified GHC.Generics as G
-- import Generics.SOP (All, DatatypeName, datatypeName, DatatypeInfo, FieldInfo(..), FieldName, ConstructorInfo(..), constructorInfo, ConstructorName, Top, All, All2, hcliftA2, hindex, hmap, hcmap, Proxy(..), SOP(..), NP(..), I(..), K(..), mapIK, hcollapse)
-- import Generics.SOP.NP (cpure_NP)
-- import Generics.SOP.Constraint (SListIN)
-- import Generics.SOP.GGP (GCode, GDatatypeInfo, GFrom, gdatatypeInfo, gfrom)

-- import qualified Data.Text as T
-- import qualified Data.Vector as V
-- import qualified Data.Map as M
-- import qualified Data.HashMap.Strict as HM

import Data.Generics.Encode.OneHot
import Data.Generics.Encode.Internal

-- $setup
-- >>> :set -XDeriveDataTypeable
-- >>> :set -XDeriveGeneric
-- >>> import Generics.SOP (Generic(..), All, Code)
-- >>> import Generics.SOP.NP
-- >>> import qualified GHC.Generics as G










