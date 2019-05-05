{-# language
    DeriveGeneric
  , DataKinds
  , FlexibleContexts
  , GADTs
  , OverloadedStrings
  , TypeOperators
  , DefaultSignatures
  , ScopedTypeVariables
  , TypeSynonymInstances
  , FlexibleInstances
#-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module Data.Generics.Encode.Val2 where

import qualified GHC.Generics as G
import Generics.SOP (All, DatatypeName, datatypeName, DatatypeInfo, FieldInfo(..), FieldName, ConstructorInfo(..), constructorInfo, Top, All, All2, hcliftA2, hcmap, Proxy(..), SOP(..), NP(..), I(..), K(..), mapIK, hcollapse)
-- import Generics.SOP.NP (cpure_NP)
-- import Generics.SOP.Constraint (SListIN)
import Generics.SOP.GGP (GCode, GDatatypeInfo, GFrom, gdatatypeInfo, gfrom)

import Data.Hashable (Hashable(..))
import qualified Data.Text as T
-- import qualified Data.Vector as V
-- import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.GenericTrie as GT

import Data.Generics.Encode.OneHot


type Path = [String]

data Val =
    VProd Path String (HM.HashMap String Val) -- ^ product
  | VSum  Path String Val                     -- ^ sum
  | VOH   Path String (OneHot Int)            -- ^ 1-hot
  | VInt  Int
  deriving (Eq, Show, G.Generic)

class ToVal a where
  {-# MINIMAL toVal #-}
  toVal :: a -> Val
  default toVal ::
    (G.Generic a, All2 ToVal (GCode a), GFrom a, GDatatypeInfo a) => a -> Val
  toVal x = sopToVal (gdatatypeInfo (Proxy :: Proxy a)) (gfrom x)  


sopToVal :: (All2 ToVal xss) => DatatypeInfo xss -> SOP I xss -> Val
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
  -- Infix cn _ _ -> undefined
-- mkVal (Infix cn _ _) xs _ _ = Con cn $ npToVals xs
    Constructor cn
      | null cns  -> VOH [] tyn oh
      | otherwise -> VProd [] cn $ mkAnonProd xs
    Record _ fi   -> VProd [] tyn $ mkProd fi xs
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

npToVals :: (All ToVal xs) => NP I xs -> [Val]
npToVals xs = hcollapse $ hcmap (Proxy :: Proxy ToVal) (mapIK toVal) xs

labels :: [String]
labels = map (('_' :) . show) [1 ..]


data A0 = A0 deriving (Eq, Show, G.Generic)
instance ToVal A0
data A = A Int deriving (Eq, Show, G.Generic)
instance ToVal A
data A2 = A2 { a2 :: Int} deriving (Eq, Show, G.Generic)
instance ToVal A2
data B = B Int Char deriving (Eq, Show, G.Generic)
data B2 = B2 { b21 :: Int, b22 :: Char } deriving (Eq, Show, G.Generic)


instance ToVal Int where toVal = VInt
