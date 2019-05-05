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
module Data.Generics.Encode.Val where

import qualified GHC.Generics as G
import Generics.SOP (All, DatatypeName, datatypeName, DatatypeInfo, FieldInfo(..), FieldName, ConstructorInfo(..), constructorInfo, Top, All, All2, hcliftA2, hcmap, Proxy(..), SOP(..), NP(..), I(..), K(..), mapIK, hcollapse)
-- import Generics.SOP.NP (cpure_NP)
-- import Generics.SOP.Constraint (SListIN)
import Generics.SOP.GGP (GCode, GDatatypeInfo, GFrom, gdatatypeInfo, gfrom)

import qualified Data.Text as T
-- import qualified Data.Vector as V
-- import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.GenericTrie as GT

import Data.Generics.Encode.OneHot


-- $setup
-- >>> :set -XDeriveDataTypeable
-- >>> :set -XDeriveGeneric
-- >>> import Generics.SOP (Generic(..), All, Code)
-- >>> import Generics.SOP.NP
-- >>> import qualified GHC.Generics as G


-- newtype Trie a = Trie { unTrie :: GT.Trie [String] a } deriving (Show)

-- insert :: [String] -> a -> Trie a -> Trie a
-- insert k v t = Trie $ GT.insert k v (unTrie t)

-- empty :: Trie a 
-- empty = Trie GT.empty


data W =
    WProd [String] (HM.HashMap String W) -- ^ product
  | WSum  [String] W                     -- ^ sum
  | WOH   [String] (OneHot Int)          -- ^ 1-hot
  | WInt  Int
  deriving (Eq, Show, G.Generic)



data Val =
    Con FieldName [Val]  -- ^ Constructor (1 or more anonymous fields)
  | Enum DatatypeName (OneHot Int)  -- ^ Enum (Constructor with 0 fields)
  | Rec (HM.HashMap FieldName Val) -- ^ Record (1 or more named fields)
  | VInt Int
  | VFloat Float
  | VChar Char
  | VString String 
  | VText T.Text
  deriving (Eq, Show, G.Generic)

class ToVal a where
  {-# MINIMAL toVal #-}
  toVal :: a -> Val
  default toVal :: (G.Generic a, All Top (GCode a), All2 ToVal (GCode a), GFrom a, GDatatypeInfo a) => a -> Val
  toVal x = sopToVal (gdatatypeInfo (Proxy :: Proxy a)) (gfrom x)  

-- instance ToVal a => ToVal (Maybe a) where
--   toVal mx = case mx of
--     Nothing -> VMaybe Nothing
--     Just x  -> VMaybe (Just $ toVal x)
-- instance (ToVal l, ToVal r) => ToVal (Either l r)
-- instance (ToVal l, ToVal r) => ToVal (l, r)

instance ToVal a => ToVal (Maybe a) where
    toVal Nothing  = Con "Nothing" []
    toVal (Just x) = Con "Just" [toVal x]

instance (ToVal a, ToVal b) => ToVal (Either a b) where
    toVal (Left x)  = Con "Left"  [toVal x]
    toVal (Right y) = Con "Right" [toVal y]

instance (ToVal a, ToVal b) => ToVal (a, b) where
    toVal (a, b) = Con "," [toVal a, toVal b]    


instance ToVal Int where toVal = VInt
instance ToVal Float where toVal = VFloat
instance ToVal Char where toVal = VChar
instance ToVal String where toVal = VString
-- instance ToVal a => ToVal [a]
instance ToVal T.Text where toVal = VText




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



npToVals :: (All ToVal xs) => NP I xs -> [Val]
npToVals xs = hcollapse $ hcmap (Proxy :: Proxy ToVal) (mapIK toVal) xs









-- λ> gdatatypeInfo (Proxy :: Proxy A)
-- ADT "Data.Generics.Encode.Val" "A" (Constructor "A" :* Nil)
data A = A Int Char deriving (Eq, Show, G.Generic)
instance ToVal A

-- ADT "Data.Generics.Encode.Val" "B" (Record "B" (FieldInfo "b1" :* FieldInfo "b2" :* Nil) :* Nil)
data B = B { b1 :: Int, b2 :: Char } deriving (Eq, Show, G.Generic)
instance ToVal B

-- ADT "Data.Generics.Encode.Val" "C" (Constructor "C1" :* Constructor "C2" :* Constructor "C3" :* Nil)
data C = C1 | C2 | C3 deriving (Eq, Show, G.Generic)
instance ToVal C

-- ADT "Data.Generics.Encode.Val" "D" (Constructor "D1" :* Constructor "D2" :* Constructor "D3" :* Nil)
data D = D1 Int | D2 Char (Maybe String) | D3 (Maybe Int) deriving (Eq, Show, G.Generic)
instance ToVal D

-- ADT "Data.Generics.Encode.Val" "E" (Record "E1" (FieldInfo "e1" :* Nil) :* Record "E2" (FieldInfo "e2" :* Nil) :* Record "E3" (FieldInfo "e3" :* Nil) :* Nil)
data E = E1 { e1 :: Int } | E2 { e2 :: Char } | E3 { e3 :: Maybe Int } deriving (Eq, Show, G.Generic)
instance ToVal E

-- Newtype "Data.Generics.Encode.Val" "F" (Constructor "F")
newtype F = F (Either Int Char) deriving (Eq, Show, G.Generic)
instance ToVal F

-- Newtype "Data.Generics.Encode.Val" "G" (Record "G" (FieldInfo "g" :* Nil))
newtype G = G { g :: Either Int (Maybe Char) } deriving (Eq, Show, G.Generic)
instance ToVal G

-- ADT "Data.Generics.Encode.Val" ":@" (Infix ":+" LeftAssociative 9 :* Infix ":-" LeftAssociative 9 :* Nil)
data a :@ b = a :+ b | a :- b deriving (Eq, Show, G.Generic)
instance (ToVal a, ToVal b) => ToVal (a :@ b)

data H = H1 C deriving (Eq, Show, G.Generic)
instance ToVal H

data J = J1 { j11 :: D, j12 :: C, j13 :: F } deriving (Eq, Show, G.Generic)
instance ToVal J

data J2 = J2 D C F deriving (Eq, Show, G.Generic)
instance ToVal J2

-- ADT "Data.Generics.Encode.Val" "L" (Constructor "L" :* Nil)
data L = L Int Char Float deriving (Eq, Show, G.Generic)
instance ToVal L


j :: J
j = J1 (D2 'z' (Just "ad")) C1 $ F (Left 42)

j2 :: J2
j2 = J2 (D2 'z' (Just "ad")) C1 $ F (Left 42)



{-
Con "J2" [
           Con "D2" [
                      VChar 'z', Con "Just" [VString "ad"]
                    ],
           Enum "C" (OH {ohDim = 3, ohIx = 0}),
           Con "F" [
                     Con "Left" [
                                  VInt 42]]]
-}

