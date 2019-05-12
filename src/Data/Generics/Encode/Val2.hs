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
  , LambdaCase
#-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Data.Generics.Encode.Val2 where

import qualified GHC.Generics as G
import Generics.SOP (All, DatatypeName, datatypeName, DatatypeInfo, FieldInfo(..), FieldName, ConstructorInfo(..), constructorInfo, Top, All, All2, hcliftA2, hcmap, Proxy(..), SOP(..), NP(..), I(..), K(..), mapIK, hcollapse, SListI(..))
-- import Generics.SOP.NP (cpure_NP)
-- import Generics.SOP.Constraint (SListIN)
import Generics.SOP.GGP (GCode, GDatatypeInfo, GFrom, gdatatypeInfo, gfrom)

-- import Data.Hashable (Hashable(..))
-- import qualified Data.Text as T
-- import qualified Data.Vector as V
-- import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.GenericTrie as GT

import Data.Generics.Encode.OneHot


-- type Path = [String]


-- | The String parameter contains the type name at the given level
data Val =
    VProd String (HM.HashMap String Val) -- ^ product
  | VSum  String (HM.HashMap String Val) -- ^ sum
  | VOH   String (OneHot Int)            -- ^ 1-hot
  | VInt  Int
  | VChar Char
  | VString String 
  deriving (Eq, Show)

-- | NOTE: if your type has a 'G.Generic' instance you can just declare an empty instance of 'ToVal' for it.
--
-- example:
--
-- @
-- data A = A Int Char deriving (G.Generic)
-- instance ToVal A
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
  -- Infix cn _ _ -> undefined
-- mkVal (Infix cn _ _) xs _ _ = Con cn $ npToVals xs
    Constructor cn
      | null cns  -> VOH tyn oh
      | otherwise -> VProd cn  $ mkAnonProd xs
    Record _ fi   -> VProd tyn $ mkProd fi xs
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


data A0 = A0 deriving (Eq, Show, G.Generic)
instance ToVal A0
data A = A Int deriving (Eq, Show, G.Generic)
instance ToVal A
data A2 = A2 { a2 :: Int } deriving (Eq, Show, G.Generic)
instance ToVal A2
data B = B Int Char deriving (Eq, Show, G.Generic)
instance ToVal B
data B2 = B2 { b21 :: Int, b22 :: Char } deriving (Eq, Show, G.Generic)
instance ToVal B2
data C = C1 | C2 | C3 deriving (Eq, Show, G.Generic)
instance ToVal C
data D = D (Maybe Int) (Either Int String) deriving (Eq, Show, G.Generic)
instance ToVal D


instance ToVal Int where toVal = VInt
instance ToVal Char where toVal = VChar
instance ToVal String where toVal = VString
instance ToVal a => ToVal (Maybe a) where
  toVal = \case
    Nothing -> VSum "Maybe" HM.empty
    Just x  -> VSum "Maybe" $ HM.singleton "Just" $ toVal x
  
instance (ToVal a, ToVal b) => ToVal (Either a b) where
  toVal = \case
    Left  l -> VSum "Either" $ HM.singleton "Left" $ toVal l
    Right r -> VSum "Either" $ HM.singleton "Right" $ toVal r

instance (ToVal a, ToVal b) => ToVal (a, b) where
  toVal (x, y) = VProd "*" $ HM.fromList $ zip labels [toVal x, toVal y]
