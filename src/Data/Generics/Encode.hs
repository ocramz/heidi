{-# language
    DeriveGeneric
  , DataKinds
  , FlexibleContexts
  , GADTs
  , OverloadedStrings
  , TypeOperators
  , DefaultSignatures
  , ScopedTypeVariables
#-}
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
-----------------------------------------------------------------------------
module Data.Generics.Encode where

import qualified GHC.Generics as G
import Generics.SOP (All, DatatypeName, datatypeName, DatatypeInfo, FieldInfo(..), FieldName, ConstructorInfo(..), constructorInfo, ConstructorName, Top, All, All2, hcliftA2, hindex, hmap, hcmap, Proxy(..), SOP(..), NP(..), I(..), K(..), mapIK, hcollapse)
-- import Generics.SOP.NP (cpure_NP)
-- import Generics.SOP.Constraint (SListIN)
import Generics.SOP.GGP (GCode, GDatatypeInfo, GFrom, gdatatypeInfo, gfrom)

import qualified Data.Text as T
-- import qualified Data.Vector as V
-- import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

-- $setup
-- >>> :set -XDeriveDataTypeable
-- >>> :set -XDeriveGeneric
-- >>> import Generics.SOP (Generic(..), All, Code)
-- >>> import Generics.SOP.NP
-- >>> import qualified GHC.Generics as G

{-| alternative ADT representation

data VRep =
    VProduct DatatypeName (HM.HashMap FieldName VRep)
  | VSum DatatypeName FieldName VRep
  | VEnum DatatypeName (OneHot Int)
  deriving (Eq, Show)

depth-first traversal to extract prefix lists
e.g. [(DatatypeName, FieldName), ... , (DatatypeName, OneHot Int)]
-}

-- data TypeMD =
--   EnumMD DatatypeName Int

data EnumMD = EnumMD DatatypeName Int deriving (Eq, Show, G.Generic)



data Val =
    Con FieldName [Val]  -- ^ Constructor (1 or more anonymous fields)
  | Enum DatatypeName (OneHot Int)  -- ^ Enum (Constructor with 0 fields)
  | Rec (HM.HashMap FieldName Val) -- ^ Record (1 or more named fields)
  | VMaybe (Maybe Val)  -- ^ Maybe   
  | VInt Int  
  | VChar Char
  | VText T.Text
  deriving (Eq, Show, G.Generic)

class ToVal a where
  toVal :: a -> Val 
  default toVal :: (G.Generic a, All Top (GCode a), All2 ToVal (GCode a), GFrom a, GDatatypeInfo a) => a -> Val
  toVal x = sopToVal (gdatatypeInfo (Proxy :: Proxy a)) (gfrom x)  

instance ToVal a => ToVal (Maybe a) where
  toVal mx = case mx of
    Nothing -> VMaybe Nothing
    Just x  -> VMaybe (Just $ toVal x)
instance (ToVal l, ToVal r) => ToVal (Either l r)
instance (ToVal l, ToVal r) => ToVal (l, r)

instance ToVal Int where toVal = VInt
instance ToVal Char where toVal = VChar
instance ToVal T.Text where toVal = VText

-- instance ToVal Val where
--     toVal = id


{- | Examples :
* `basic-sop` - generic show function  :
https://hackage.haskell.org/package/basic-sop-0.2.0.2/docs/src/Generics-SOP-Show.html#gshow
* `tree-diff` - single-typed ADT reconstruction :
http://hackage.haskell.org/package/tree-diff-0.0.2/docs/src/Data.TreeDiff.Class.html#sopToExpr
-}

sopToVal :: (All2 ToVal xss, All Top xss) => DatatypeInfo xss -> SOP I xss -> Val
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
mkVal (Infix cn _ _) xs _ _ = Con cn $ npToVals xs
mkVal (Constructor cn) xs tyn oh
  | null cns  = Enum tyn oh
  | otherwise = Con cn cns
  where
    cns = npToVals xs
mkVal (Record _ fi) xs _ _ = Rec $ HM.fromList $ hcollapse $ hcliftA2 (Proxy :: Proxy ToVal) mk fi xs
  where
    mk :: ToVal v => FieldInfo v -> I v -> K (FieldName, Val) v
    mk (FieldInfo n) (I x) = K (n, toVal x)

-- λ> mkOH (gdatatypeInfo (Proxy :: Proxy C)) (gfrom C2)
-- OH {ohDim = 3, ohIx = 1}
mkOH :: (All Top xs) =>
        DatatypeInfo xs -> SOP I xs -> OneHot Int
mkOH di sop = oneHot where
     oneHot = OH sdim six
     six = hindex sop
     sdim = length $ constructorList di

data OneHot i = OH { ohDim :: i, ohIx :: i } deriving (Eq, Show)     

npToVals :: (All ToVal xs) => NP I xs -> [Val]
npToVals xs = hcollapse $ hcmap (Proxy :: Proxy ToVal) (mapIK toVal) xs

constructorList :: All Top xs => DatatypeInfo xs -> [ConstructorName]
constructorList di = hcollapse $ hmap (\(Constructor x) -> K x) $ constructorInfo di    







-- λ> gdatatypeInfo (Proxy :: Proxy A)
-- ADT "Core.Data.Frame.Generic" "A" (Constructor "A" :* Nil)
data A = A Int Char deriving (Eq, Show, G.Generic)
instance ToVal A

-- ADT "Core.Data.Frame.Generic" "B" (Record "B" (FieldInfo "b1" :* FieldInfo "b2" :* Nil) :* Nil)
data B = B { b1 :: Int, b2 :: Char } deriving (Eq, Show, G.Generic)
instance ToVal B

-- ADT "Core.Data.Frame.Generic" "C" (Constructor "C1" :* Constructor "C2" :* Constructor "C3" :* Nil)
data C = C1 | C2 | C3 deriving (Eq, Show, G.Generic)
instance ToVal C

-- ADT "Core.Data.Frame.Generic" "D" (Constructor "D1" :* Constructor "D2" :* Constructor "D3" :* Nil)
data D = D1 Int | D2 Char | D3 (Maybe Int) deriving (Eq, Show, G.Generic)
instance ToVal D

-- ADT "Core.Data.Frame.Generic" "E" (Record "E1" (FieldInfo "e1" :* Nil) :* Record "E2" (FieldInfo "e2" :* Nil) :* Record "E3" (FieldInfo "e3" :* Nil) :* Nil)
data E = E1 { e1 :: Int } | E2 { e2 :: Char } | E3 { e3 :: Maybe Int } deriving (Eq, Show, G.Generic)
instance ToVal E

-- Newtype "Core.Data.Frame.Generic" "F" (Constructor "F")
newtype F = F (Either Int Char) deriving (Eq, Show, G.Generic)
instance ToVal F

-- Newtype "Core.Data.Frame.Generic" "G" (Record "G" (FieldInfo "g" :* Nil))
newtype G = G { g :: Either Int (Maybe Char) } deriving (Eq, Show, G.Generic)
instance ToVal G

-- ADT "Core.Data.Frame.Generic" ":@" (Infix ":+" LeftAssociative 9 :* Infix ":-" LeftAssociative 9 :* Nil)
data a :@ b = a :+ b | a :- b deriving (Eq, Show, G.Generic)
instance (ToVal a, ToVal b) => ToVal (a :@ b)

data H = H1 C deriving (Eq, Show, G.Generic)
instance ToVal H

data J = J1 { j11 :: D, j12 :: C, j13 :: F } deriving (Eq, Show, G.Generic)
instance ToVal J











