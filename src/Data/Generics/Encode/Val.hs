{-# language
    DeriveGeneric
  , FlexibleContexts
  , GADTs
  , OverloadedStrings
  , DefaultSignatures
  , ScopedTypeVariables
  , FlexibleInstances
  , LambdaCase
#-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Encode.Val
-- Description :  Generic encoding of algebraic datatypes
-- Copyright   :  (c) Marco Zocca (2019)
-- License     :  MIT
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Generic encoding of algebraic datatypes, using 'generics-sop'
--
-- Examples, inspiration and code borrowed from :
-- 
-- * @basic-sop@ - generic show function : https://hackage.haskell.org/package/basic-sop-0.2.0.2/docs/src/Generics-SOP-Show.html#gshow
-- 
-- * @tree-diff@ - single-typed ADT reconstruction : http://hackage.haskell.org/package/tree-diff-0.0.2/docs/src/Data.TreeDiff.Class.html#sopToExpr
-----------------------------------------------------------------------------
module Data.Generics.Encode.Val (gflatten,
                                 -- * VP (Primitive types)
                                 VP(..),
                                 getInt, getFloat, getDouble, getChar, getString, getText, getOH,
                                 -- * TC (Type and Constructor annotation)
                                 TC(..), tcTyN, tcTyCon, 
                                 -- * ToVal (generic ADT encoding)
                                 ToVal(..), Val) where

import qualified GHC.Generics as G
import Generics.SOP (All, DatatypeName, datatypeName, DatatypeInfo, FieldInfo(..), FieldName, ConstructorInfo(..), constructorInfo, All, All2, hcliftA2, hcmap, Proxy(..), SOP(..), NP(..), I(..), K(..), mapIK, hcollapse)
-- import Generics.SOP.NP (cpure_NP)
-- import Generics.SOP.Constraint (SListIN)
import Generics.SOP.GGP (GCode, GDatatypeInfo, GFrom, gdatatypeInfo, gfrom)

import Data.Hashable (Hashable(..))
-- import qualified Data.Text as T ()
import Data.Text (Text)
-- import qualified Data.Vector as V
-- import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
-- import qualified Data.GenericTrie as GT
import Data.Generics.Encode.OneHot
-- import Data.List (unfoldr)
-- import qualified Data.Foldable as F
-- import qualified Data.Sequence as S (Seq(..), empty)
-- import Data.Sequence ((<|), (|>))
import Prelude hiding (getChar)

-- $setup
-- >>> :set -XDeriveDataTypeable
-- >>> :set -XDeriveGeneric
-- >>> import Generics.SOP (Generic(..), All, Code)
-- >>> import Generics.SOP.NP
-- >>> import qualified GHC.Generics as G


-- | Flatten a value into a 1-layer hashmap, via the value's generic encoding
gflatten :: ToVal a => a -> HM.HashMap [TC] VP
gflatten = flatten . toVal


-- | A (type, constructor) name pair
data TC = TC String String deriving (Eq, Show, Ord, G.Generic)
instance Hashable TC

-- | Type name
tcTyN :: TC -> String
tcTyN (TC n _) = n
-- | Type constructor
tcTyCon :: TC -> String
tcTyCon (TC _ c) = c

-- | Fold a 'Val' into a 1-layer hashmap indexed by the input value's (type, constructor) metadata
flatten :: Val -> HM.HashMap [TC] VP
flatten = go ([], HM.empty) where
  go (ks, hmacc) = \case
    VRec ty hm     -> HM.foldlWithKey' (\hm' k t -> go (TC ty k : ks, hm') t) hmacc hm
    VOH   ty cn oh -> insRev (TC ty cn : ks) (VPOH oh) hmacc
    VPrim vp       -> insRev ks vp hmacc

-- | Reverse keys list and use that to insert item
insRev :: (Eq a, Hashable a) => [a] -> v -> HM.HashMap [a] v -> HM.HashMap [a] v
insRev ks = HM.insert (reverse ks)


-- | Primitive types
data VP =
    VPInt    Int 
  | VPFloat  Float
  | VPDouble Double
  | VPChar   Char
  | VPString String
  | VPText   Text 
  | VPOH     (OneHot Int)
  deriving (Eq, Show)

-- | Extract an Int
getInt :: VP -> Maybe Int
getInt = \case {VPInt i -> Just i; _ -> Nothing}
-- | Extract a Float
getFloat :: VP -> Maybe Float
getFloat = \case {VPFloat i -> Just i; _ -> Nothing}
-- | Extract a Double
getDouble :: VP -> Maybe Double
getDouble = \case {VPDouble i -> Just i; _ -> Nothing}
-- | Extract a Char
getChar :: VP -> Maybe Char
getChar = \case {VPChar i -> Just i; _ -> Nothing}
-- | Extract a String
getString :: VP -> Maybe String
getString = \case {VPString i -> Just i; _ -> Nothing}
-- | Extract a Text string
getText :: VP -> Maybe Text
getText = \case {VPText i -> Just i; _ -> Nothing}
-- | Extract a OneHot value
getOH :: VP -> Maybe (OneHot Int)
getOH = \case {VPOH i -> Just i; _ -> Nothing}


-- | The String parameter contains the type name at the given level
data Val =
    VRec   String        (HM.HashMap String Val) -- ^ recursion
  | VOH    String String (OneHot Int)            -- ^ 1-hot
  | VPrim  VP                                    -- ^ primitive types
  deriving (Eq, Show)




-- | NOTE: if your type has a 'G.Generic' instance you just need to declare an empty instance of 'ToVal' for it (a default implementation of 'toVal' is provided).
--
-- example:
--
-- @
-- data A = A Int Char deriving ('G.Generic')
-- instance 'ToVal' A
-- @
class ToVal a where
  toVal :: a -> Val
  default toVal ::
    (G.Generic a, All2 ToVal (GCode a), GFrom a, GDatatypeInfo a) => a -> Val
  toVal x = sopToVal (gdatatypeInfo (Proxy :: Proxy a)) (gfrom x)  


sopToVal :: All2 ToVal xss => DatatypeInfo xss -> SOP I xss -> Val
sopToVal di sop@(SOP xss) = hcollapse $ hcliftA2
    (Proxy :: Proxy (All ToVal))
    (\ci xs -> K (mkVal ci xs tyName oneHot))
    (constructorInfo di)
    xss
  where
     tyName = datatypeName di
     oneHot = mkOH di sop
     
mkVal :: All ToVal xs =>
         ConstructorInfo xs -> NP I xs -> DatatypeName -> OneHot Int -> Val
mkVal cinfo xs tyn oh = case cinfo of
    Infix cn _ _  -> VRec cn (mkAnonProd xs)
    Constructor cn
      | null cns  -> VOH tyn cn oh
      | otherwise -> VRec cn  $ mkAnonProd xs
    Record _ fi   -> VRec tyn $ mkProd fi xs
  where
    cns :: [Val]
    cns = npToVals xs

mkProd :: All ToVal xs => NP FieldInfo xs -> NP I xs -> HM.HashMap String Val
mkProd fi xs = HM.fromList $ hcollapse $ hcliftA2 (Proxy :: Proxy ToVal) mk fi xs where
  mk :: ToVal v => FieldInfo v -> I v -> K (FieldName, Val) v
  mk (FieldInfo n) (I x) = K (n, toVal x)

mkAnonProd :: All ToVal xs => NP I xs -> HM.HashMap String Val
mkAnonProd xs = HM.fromList $ zip labels cns where
  cns = npToVals xs

npToVals :: All ToVal xs => NP I xs -> [Val]
npToVals xs = hcollapse $ hcmap (Proxy :: Proxy ToVal) (mapIK toVal) xs

-- | >>> take 3 labels
-- ["_0","_1","_2"]
labels :: [String]
labels = map (('_' :) . show) [0 ..]


instance ToVal Int where toVal = VPrim . VPInt
instance ToVal Float where toVal = VPrim . VPFloat
instance ToVal Double where toVal = VPrim . VPDouble
instance ToVal Char where toVal = VPrim . VPChar
instance ToVal String where toVal = VPrim . VPString
instance ToVal Text where toVal = VPrim . VPText

instance ToVal a => ToVal (Maybe a) where
  toVal = \case
    Nothing -> VRec "Maybe" HM.empty
    Just x  -> VRec "Maybe" $ HM.singleton "Just" $ toVal x
  
instance (ToVal a, ToVal b) => ToVal (Either a b) where
  toVal = \case
    Left  l -> VRec "Either" $ HM.singleton "Left" $ toVal l
    Right r -> VRec "Either" $ HM.singleton "Right" $ toVal r

instance (ToVal a, ToVal b) => ToVal (a, b) where
  toVal (x, y) = VRec "*" $ HM.fromList $ zip labels [toVal x, toVal y]         








-- examples

data A0 = A0 deriving (Eq, Show, G.Generic)
instance ToVal A0
newtype A = A Int deriving (Eq, Show, G.Generic)
instance ToVal A
newtype A2 = A2 { a2 :: Int } deriving (Eq, Show, G.Generic)
instance ToVal A2
data B = B Int Char deriving (Eq, Show, G.Generic)
instance ToVal B
data B2 = B2 { b21 :: Int, b22 :: Char } deriving (Eq, Show, G.Generic)
instance ToVal B2
data C = C1 | C2 | C3 deriving (Eq, Show, G.Generic)
instance ToVal C
data D = D (Maybe Int) (Either Int String) deriving (Eq, Show, G.Generic)
instance ToVal D
data E = E (Maybe Int) (Maybe Char) deriving (Eq, Show, G.Generic)
instance ToVal E
newtype F = F (Int, Char) deriving (Eq, Show, G.Generic)
instance ToVal F

