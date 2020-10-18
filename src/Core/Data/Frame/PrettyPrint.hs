{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language ConstraintKinds #-}
{-# language DeriveAnyClass #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# options_ghc -Wno-unused-imports #-}
module Core.Data.Frame.PrettyPrint where

import Data.Proxy (Proxy)
import qualified GHC.Generics as G
import qualified Data.Foldable as F (foldl', foldlM)

import Data.Function (on)
import Data.List (filter, sortBy, groupBy, intersperse)

-- boxes
import Text.PrettyPrint.Boxes (Box, Alignment, emptyBox, nullBox, vcat, hcat, vsep, hsep, text, para, punctuateH, render, printBox, (<>), (<+>), (//), (/+/), top, left, right, center1, center2, rows, cols)

-- import qualified Data.Text as T
-- containers
import qualified Data.Map as M
-- generic-trie
import qualified Data.GenericTrie as GT
-- generics-sop
import Generics.SOP (All, HasDatatypeInfo(..), datatypeInfo, DatatypeName, datatypeName, DatatypeInfo(..), FieldInfo(..), FieldName, fieldName, ConstructorInfo(..), constructorInfo, ConstructorName, constructorName, All(..), All2, hcliftA, hcliftA2, hliftA, hcmap, Proxy(..), SOP(..), NP(..), I(..), K(..), unK, mapIK, hcollapse, SListI, hcpure)
import Generics.SOP.NP (cpure_NP)
-- import Generics.SOP.Constraint (SListIN)
import Generics.SOP.GGP (GCode, GDatatypeInfo, GFrom, gdatatypeInfo, gfrom)
-- hashable
import Data.Hashable (Hashable(..))
-- unordered-containers
import qualified Data.HashMap.Strict as HM (HashMap, fromList, toList, union, keys, mapWithKey)

import qualified Core.Data.Frame as CDF
import qualified Core.Data.Frame.Generic as CDF (encode)
import Data.Generics.Encode.Internal (Heidi, toVal, Val(..), VP(..))
import qualified Data.Generics.Encode.OneHot as OH (OneHot)

-- import Prelude hiding ((<>))

{-
+-------------+-----------------+
| Person      | House           |
+-------+-----+-------+---------+
| Name  | Age | Color | Price   |
+-------+-----+-------+---------+
| David | 63  | Green | $170000 |
| Ava   | 34  | Blue  | $115000 |
| Sonic | 12  | Green | $150000 |
+-------+-----+-------+---------+

(table example from colonnade : https://hackage.haskell.org/package/colonnade-1.2.0.2/docs/src/Colonnade.html#cap )
-}


-- render the frame header
--
-- λ> printBox $ header $ toVal (E (Just 32) Nothing)
--       E
-- -------------
--   _0  |   _1
-- Maybe   Maybe
--  ----
--  Just
--   ()
-- λ>
header :: Val -> Box
header = \case
  VRec ty hm ->
    let
      bxs = values $ HM.mapWithKey (\k v -> text k /|/ header v) hm
    in
      -- dashesP bxs /|/ hSepList bxs
      text ty /|/ dashesP bxs /|/ hSepList bxs
  VPrim _ -> text "()"
  _ -> undefined -- TODO

values :: HM.HashMap k v -> [v]
values = map snd . HM.toList

dashesP :: [Box] -> Box
dashesP = punctuateH top (text "+") . map (\b -> dashes (cols b + 2))

dashes :: Int -> Box
dashes n = text $ replicate n '-'

hSepList :: [Box] -> Box
hSepList = hcat top . intersperse seph

seph :: Box
seph = text " | "

(/|/) :: Box -> Box -> Box
b1 /|/ b2 = vcat center1 [b1, b2]


data Z = Z Int Int
instance Show Z where
  show (Z a b) = unwords [show a, "\n", show b]

-- examples

data A0 = A0 deriving (Eq, Show, G.Generic)
data A = A Int deriving (Eq, Show, G.Generic, HasHeader)
newtype A' = A' Int deriving (Eq, Show, G.Generic, HasHeader)
newtype A2 = A2 { a2 :: Int } deriving (Eq, Show, G.Generic, HasHeader)
data B = B Int Char deriving (Eq, Show, G.Generic, HasHeader)
data B2 = B2 { b21 :: Int, b22 :: Char } deriving (Eq, Show, G.Generic, HasHeader)
data C = C1 Int | C2 A | C3 () deriving (Eq, Show, G.Generic, HasHeader)
data C2 = C21 {c21a :: Int, c21b :: ()} | C22 {c22 :: A} | C23 () deriving (Eq, Show, G.Generic, HasHeader)
data D = D (Maybe Int) (Either Int String) deriving (Eq, Show, G.Generic)
data E = E (Maybe Int) (Maybe Char) deriving (Eq, Show, G.Generic)
data R = R { r1 :: B2, r2 :: C , r3 :: B } deriving (Eq, Show, G.Generic, HasHeader)


instance HasHeader Int where hasHeader _ = HPrim "Int"
instance HasHeader Char where hasHeader _ = HPrim "Char"
instance HasHeader () where hasHeader _ = HUnit
instance HasHeader String where hasHeader _ = HPrim "String"

newtype HProduct = HProduct {
  getHProduct :: HM.HashMap String Header
  } deriving (Eq)
instance Show HProduct where show = show . getHProduct

data Header =
     HSum String (HM.HashMap String HProduct) -- ^ products
   | HPrim String -- ^ primitive types
   | HUnit
   deriving (Eq, Show)

-- λ>  hasHeader (Proxy :: Proxy C2)
-- HSum "C2" (fromList [
--               ("C21",fromList [
--                   ("c21b",HUnit),
--                   ("c21a",HPrim "Int")]),
--               ("C23",fromList [
--                   ("_0",HUnit)]),
--               ("C22",fromList [
--                   ("c22",HSum "A" (fromList [
--                                       ("A",fromList [
--                                           ("_0",HPrim "Int")])]))])])

class HasHeader a where
  hasHeader :: Proxy a -> Header
  default hasHeader ::
    (G.Generic a, All2 HasHeader (GCode a), GDatatypeInfo a) => Proxy a -> Header
  hasHeader _ = hasHeader' (gdatatypeInfo (Proxy :: Proxy a))


hasHeader' :: (All2 HasHeader xs, SListI xs) => DatatypeInfo xs -> Header
hasHeader' di = HSum dtn $ HM.fromList $ hcollapse $ hcliftA allp goConstructor cinfo
  where
    cinfo = constructorInfo di
    dtn = datatypeName di

goConstructor :: forall xs . (All HasHeader xs) => ConstructorInfo xs -> K (String, HProduct) xs
goConstructor = \case
  Record n ns -> K (n, mkProd ns)
  Constructor n -> K (n, mkAnonProd (Proxy @xs) )
  Infix n _ _ -> K (n, mkAnonProd (Proxy @xs) )


-- | anonymous products
mkAnonProd :: forall xs. (SListI xs, All HasHeader xs) => Proxy xs -> HProduct
mkAnonProd _ =
  HProduct $ HM.fromList $ zip labels $ hcollapse (hcpure p hasHeaderK :: NP (K Header) xs)
  where
    labels :: [String]
    labels = map (('_' :) . show) ([0 ..] :: [Int])
    hasHeaderK :: forall a. HasHeader a => K Header a
    hasHeaderK = K (hasHeader (Proxy @a))

-- | products
mkProd :: All HasHeader xs => NP FieldInfo xs -> HProduct
mkProd finfo = HProduct $ HM.fromList $ hcollapse $ hcliftA p goField finfo

goField :: forall a . (HasHeader a) => FieldInfo a -> K (String, Header) a
goField (FieldInfo n) = goFieldAnon n

goFieldAnon :: forall a . HasHeader a => String -> K (String, Header) a
goFieldAnon n = K (n, hasHeader (Proxy @a))

allp :: Proxy (All HasHeader)
allp = Proxy

p :: Proxy HasHeader
p = Proxy



{-
gshow :: forall a. (Generic a, HasDatatypeInfo a, All2 Show (Code a))
      => a -> String
gshow a =
  gshow' (constructorInfo (datatypeInfo (Proxy :: Proxy a))) (from a)

gshow' :: (All2 Show xss, SListI xss) => NP ConstructorInfo xss -> SOP I xss -> String
gshow' cs (SOP sop) = hcollapse $ hcliftA2 allp goConstructor cs sop

goConstructor :: All Show xs => ConstructorInfo xs -> NP I xs -> K String xs
goConstructor (Constructor n) args =
    K $ intercalate " " (n : args')
  where
    args' :: [String]
    args' = hcollapse $ hcliftA p (K . show . unI) args

goConstructor (Record n ns) args =
    K $ n ++ " {" ++ intercalate ", " args' ++ "}"
  where
    args' :: [String]
    args' = hcollapse $ hcliftA2 p goField ns args

goConstructor (Infix n _ _) (arg1 :* arg2 :* Nil) =
    K $ show arg1 ++ " " ++ show n ++ " " ++ show arg2
#if __GLASGOW_HASKELL__ < 800
goConstructor (Infix _ _ _) _ = error "inaccessible"
#endif

goField :: Show a => FieldInfo a -> I a -> K String a
goField (FieldInfo field) (I a) = K $ field ++ " = " ++ show a

p :: Proxy Show
p = Proxy

allp :: Proxy (All Show)
allp = Proxy
-}
