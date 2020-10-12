{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# options_ghc -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Data.Generics.Encode.Internal.Simple where

import Data.Proxy (Proxy)
import qualified GHC.Generics as G

-- containers
import qualified Data.Map as M (Map, fromList, insert, lookup)
-- generics-sop
import Generics.SOP (All, HasDatatypeInfo(..), datatypeInfo, DatatypeName, datatypeName, DatatypeInfo, FieldInfo(..), FieldName, ConstructorInfo(..), constructorInfo, All, All2, hcliftA2, hcmap, Proxy(..), SOP(..), NP(..), I(..), K(..), mapIK, hcollapse)
-- import Generics.SOP.NP (cpure_NP)
-- import Generics.SOP.Constraint (SListIN)
import Generics.SOP.GGP (GCode, GDatatypeInfo, GFrom, gdatatypeInfo, gfrom)
-- hashable
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM

import Data.Generics.Encode.Internal.Prim (VP(..))

data VSP =
     VspSum  String (HM.HashMap String VSP) -- ^ sums
   | VspProd String (HM.HashMap String VSP) -- ^ products
   | VspPrim VP
   deriving (Eq, Show)

