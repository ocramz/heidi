{-# language DeriveGeneric, FlexibleContexts, ScopedTypeVariables #-}
module Data.Generics.Encode.OneHot (OneHot, onehotDim, onehotIx, mkOH) where

-- import qualified GHC.Generics as G
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

-- | Construct a 'OneHot' from generic datatype and value information
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
data OneHot i = OH { ohDim :: i, ohIx :: i } deriving (Eq, Show)

-- | Embedding dimension of the 1-hot encoded vector
onehotDim :: OneHot i -> i
onehotDim = ohDim
-- | Active ('hot') index of the 1-hot encoded vector
onehotIx :: OneHot i -> i
onehotIx = ohIx


constructorList :: SListI xs => DatatypeInfo xs -> [ConstructorName]
constructorList di = hcollapse $ hmap (\(Constructor x) -> K x) $ constructorInfo di


-- data C = C1 | C2 | C3 deriving (Eq, Show, G.Generic)
