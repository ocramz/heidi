{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
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
{-# options_ghc -Wno-unused-top-binds #-}
module Core.Data.Frame.PrettyPrint (HasHeader(..), Header(..))where

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
import qualified Data.HashMap.Strict as HM (HashMap, singleton, fromList, toList, union, keys, mapWithKey)

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


-- -- render the frame header
-- --
-- -- λ> printBox $ valBox $ toVal (E (Just 32) Nothing)
-- --       E
-- -- -------------
-- --   _0  |   _1
-- -- Maybe   Maybe
-- --  ----
-- --  Just
-- --   ()
-- -- λ>
-- valBox :: Val -> Box
-- valBox = \case
--   VRec ty hm ->
--     let
--       bxs = values $ HM.mapWithKey (\k v -> text k /|/ valBox v) hm
--     in
--       -- dashesP bxs /|/ hSepList bxs
--       text ty /|/ dashesWith '+' bxs /|/ hSepList bxs
--   VPrim _ -> text "()"
--   _ -> undefined -- TODO

headerBox :: Header String -> Box
headerBox h =
  case h of
      HSum ty hm -> withHM '+' ty hm
      HProd ty hm -> withHM '*' ty hm
      HLeaf l -> text l
  where
    withHM sep ty hm = boxLayer
      where
        boxLayer = text ty /|/
                   dashesWith sep bxs /|/
                   hSepList bxs
        bxs = values $ HM.mapWithKey (\k v -> text k /|/ headerBox v) hm



values :: HM.HashMap k v -> [v]
values = map snd . HM.toList

dashesWith :: Char -> [Box] -> Box
dashesWith sep bxs = punctuateH top (text [sep]) $  map (\b -> dashes (cols b + n)) bxs
  where n = length bxs - 1

dashes :: Int -> Box
dashes n = text $ replicate n '-'

hSepList :: [Box] -> Box
hSepList = hcat top . intersperse seph

seph :: Box
seph = text " | "

(/|/) :: Box -> Box -> Box
b1 /|/ b2 = vcat center1 [b1, b2]



-- the type param 't' can store information at the leaves, e.g. list-shaped keys for lookup
data Header t =
     HSum String (HM.HashMap String (Header t)) -- ^ sums
   | HProd String (HM.HashMap String (Header t)) -- ^ products
   | HLeaf t -- ^ primitive types
   deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasHeader Int where header _ = HLeaf "Int"
instance HasHeader Char where header _ = HLeaf "Char"
instance HasHeader () where header _ = HLeaf "()"
instance HasHeader String where header _ = HLeaf "String"
instance (HasHeader a) => HasHeader (Maybe a) where
  header _ = HSum "Maybe" $ HM.fromList [
    ("Nothing", HLeaf "_") ,
    ("Just", header (Proxy @a))
                                           ]
instance (HasHeader e, HasHeader a) => HasHeader (Either e a) where
instance (HasHeader a, HasHeader b) => HasHeader (a, b)
instance (HasHeader a, HasHeader b, HasHeader c) => HasHeader (a, b, c)

class HasHeader a where
  header :: Proxy a -> Header String
  default header ::
    (G.Generic a, All2 HasHeader (GCode a), GDatatypeInfo a) => Proxy a -> Header String
  header _ = header' (gdatatypeInfo (Proxy :: Proxy a))


header' :: (All2 HasHeader xs, SListI xs) => DatatypeInfo xs -> Header String
header' di
  | single hs = 
      let (n, hdr) = head hs
      in HProd dtn $ HM.singleton n hdr
  | otherwise = HSum dtn $ HM.fromList hs
  where
    hs :: [(String, Header String)]
    hs = hcollapse $ hcliftA allp (goConstructor dtn) cinfo
    cinfo = constructorInfo di
    dtn = datatypeName di

goConstructor :: forall xs . (All HasHeader xs) => String -> ConstructorInfo xs -> K (String, Header String) xs
goConstructor dtn = \case
  Record n ns -> K (n, mkProd dtn ns)
  Constructor n -> K (n, mkAnonProd dtn (Proxy @xs) )
  Infix n _ _ -> K (n, mkAnonProd dtn (Proxy @xs) )


-- | anonymous products
mkAnonProd :: forall xs. (SListI xs, All HasHeader xs) => String -> Proxy xs -> Header String
mkAnonProd dtn _ 
  | single hs =
      let hdr = head hs
      in hdr
  | null hs = HLeaf dtn
  | otherwise = HProd dtn $ HM.fromList $ zip labels hs
  where
    hs :: [Header String]
    hs = hcollapse (hcpure p headerK :: NP (K (Header String)) xs)
    headerK :: forall a. HasHeader a => K (Header String) a
    headerK = K (header (Proxy @a))
    labels = map (('_' :) . show) ([0 ..] :: [Int])

-- | products
mkProd :: All HasHeader xs => String -> NP FieldInfo xs -> Header String
mkProd dtn finfo -- = HProd dtn $ HM.fromList hs
  | single hs =
      let (n, hdr) = head hs
      in hdr
  | otherwise = HProd dtn $ HM.fromList hs
  where
    hs :: [(String, Header String)]
    hs = hcollapse $ hcliftA p goField finfo

goField :: forall a . (HasHeader a) => FieldInfo a -> K (String, Header String) a
goField (FieldInfo n) = goFieldAnon n

goFieldAnon :: forall a . HasHeader a => String -> K (String, Header String) a
goFieldAnon n = K (n, header (Proxy @a))

single :: [a] -> Bool
single = (== 1) . length

allp :: Proxy (All HasHeader)
allp = Proxy

p :: Proxy HasHeader
p = Proxy




-- examples

data A0 = MkA0 deriving (Eq, Show, G.Generic, HasHeader)
data A = MkA Int deriving (Eq, Show, G.Generic, HasHeader)
newtype A' = MkA' Int deriving (Eq, Show, G.Generic, HasHeader)
newtype A2 = A2 { a2 :: Int } deriving (Eq, Show, G.Generic, HasHeader)
data B = MkB Int Char deriving (Eq, Show, G.Generic, HasHeader)
data B2 = MkB2 { b21 :: Int, b22 :: Char } deriving (Eq, Show, G.Generic, HasHeader)
data C = MkC1 {c1 :: Int} | MkC2 A | MkC3 () deriving (Eq, Show, G.Generic, HasHeader)
data C2 = C21 {c21a :: Int, c21b :: ()} | C22 {c22 :: A} | C23 () deriving (Eq, Show, G.Generic, HasHeader)
data C3 a = C31 a a deriving (Eq, Show, G.Generic, HasHeader)
data D = D (Maybe Int) C deriving (Eq, Show, G.Generic, HasHeader)
data E = E (Maybe Int) (Maybe Char) deriving (Eq, Show, G.Generic, Heidi)
data R = MkR { r1 :: B2, r2 :: C , r3 :: B } deriving (Eq, Show, G.Generic, HasHeader)






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
