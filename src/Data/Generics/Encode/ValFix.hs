{-# language DeriveGeneric, DeriveFunctor, DeriveTraversable #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Data.Generics.Encode.ValFix (Val(..), ValF(..), TypeName, ConstructorName) where

import qualified GHC.Generics as G
import Generics.SOP (All, DatatypeName, datatypeName, DatatypeInfo, FieldInfo(..), FieldName, ConstructorInfo(..), constructorInfo, Top, All, All2, hcliftA2, hcmap, Proxy(..), SOP(..), NP(..), I(..), K(..), mapIK, hcollapse, SListI(..))
-- import Generics.SOP.NP (cpure_NP)
-- import Generics.SOP.Constraint (SListIN)
import Generics.SOP.GGP (GCode, GDatatypeInfo, GFrom, gdatatypeInfo, gfrom)

-- import Data.Hashable (Hashable(..))
-- import qualified Data.Text as T
-- import qualified Data.Vector as V
import qualified Data.IntMap as IM
import qualified Data.HashMap.Strict as HM
import qualified Data.GenericTrie as GT

import Data.Fix (Fix(..), cata, cataM, ana, anaM, hylo, hyloM)

import qualified Data.Generics.Encode.OneHot as OH (OneHot(..))


type TypeName = String
type ConstructorName = String

-- | Base functor of our abstract data syntax
data ValF x =
    VProd TypeName (HM.HashMap ConstructorName x) -- ^ product (e.g. records, tuples)
  | VSum  TypeName ConstructorName x              -- ^ sum (e.g. 'Maybe', 'Either')
  | VOH   TypeName (OH.OneHot Int)                   -- ^ 1-hot encoding of enums
  | VInt  Int
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Val = Val (Fix ValF) deriving (Eq, Show)

cataVal :: (ValF a -> a) -> Val -> a
cataVal phi (Val v) = cata phi v

cataValM :: Monad m => (ValF a -> m a) -> Val -> m a
cataValM phi (Val v) = cataM phi v

anaVal :: (a -> ValF a) -> a -> Val
anaVal psi z = Val $ ana psi z

anaValM :: Monad m => (a -> m (ValF a)) -> a -> m Val
anaValM psi z = Val <$> anaM psi z
