{-# language DeriveGeneric, FlexibleContexts, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Encode.OneHot
-- Description :  Generic 1-hot encoding of enumeration types 
-- Copyright   :  (c) Marco Zocca (2019)
-- License     :  MIT
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Generic 1-hot encoding of enumeration types
--
-----------------------------------------------------------------------------
module Data.Generics.Encode.OneHot (OneHot, onehotDim, onehotIx, oneHotV
                                   -- ** Internal
                                   , mkOH) where

import qualified GHC.Generics as G
import Data.Hashable (Hashable(..))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Generics.SOP (DatatypeInfo, ConstructorInfo(..), constructorInfo, ConstructorName, hindex, hmap, SOP(..), I(..), K(..), hcollapse, SListI)
-- import Generics.SOP.NP (cpure_NP)
-- import Generics.SOP.Constraint (SListIN)
-- import Generics.SOP.GGP (GCode, GDatatypeInfo, GFrom, gdatatypeInfo, gfrom)

-- $setup
-- >>> :set -XDeriveDataTypeable
-- >>> :set -XDeriveGeneric
-- >>> import Generics.SOP (Generic(..), All, Code, Proxy(..))
-- >>> import Generics.SOP.NP
-- >>> import qualified GHC.Generics as G
-- >>> import Generics.SOP.GGP (gdatatypeInfo, gfrom)
-- >>> data C = C1 | C2 | C3 deriving (Eq, Show, G.Generic)

-- | Construct a 'OneHot' encoding from generic datatype and value information
-- 
-- >>> mkOH (gdatatypeInfo (Proxy :: Proxy C)) (gfrom C2)
-- OH {ohDim = 3, ohIx = 1}
mkOH :: SListI xs => DatatypeInfo xs -> SOP I xs -> OneHot Int
mkOH di sop = oneHot where
     oneHot = OH sdim six
     six = hindex sop
     sdim = length $ constructorList di

-- | 1-hot encoded vector.
--
-- This representation is used to encode categorical variables as points in a vector space.
data OneHot i = OH {
  ohDim :: i -- ^ Dimensionality of the ambient space
  , ohIx :: i  -- ^ index of '1'
  } deriving (Eq, Ord, G.Generic)
instance Hashable i => Hashable (OneHot i)
instance Show i => Show (OneHot i) where
  show (OH od oi) = concat ["OH_", show od, "_", show oi]

-- | Embedding dimension of the 1-hot encoded vector
onehotDim :: OneHot i -> i
onehotDim = ohDim
-- | Active ('hot') index of the 1-hot encoded vector
onehotIx :: OneHot i -> i
onehotIx = ohIx

constructorList :: SListI xs => DatatypeInfo xs -> [ConstructorName]
constructorList di = hcollapse $ hmap (\(Constructor x) -> K x) $ constructorInfo di


-- | Create a one-hot vector
oneHotV :: Num a =>
           OneHot Int
        -> V.Vector a
oneHotV (OH n i) = V.create $ do
  vm <- VM.replicate n 0
  VM.write vm i 1
  return vm


-- data C = C1 | C2 | C3 deriving (Eq, Show, G.Generic)
