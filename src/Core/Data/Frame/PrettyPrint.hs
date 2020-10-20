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
module Core.Data.Frame.PrettyPrint where

import Data.Proxy (Proxy)
import qualified GHC.Generics as G
import qualified Data.Foldable as F (foldl', foldlM)

import Data.Foldable (Foldable(..))
import Data.Function (on)
import Data.List (filter, sortBy, groupBy, intersperse)

-- boxes
import Text.PrettyPrint.Boxes (Box, Alignment, emptyBox, nullBox, vcat, hcat, vsep, hsep, text, para, punctuateH, render, printBox, (<>), (<+>), (//), (/+/), top, left, right, center1, center2, rows, cols, moveDown)

-- import qualified Data.Text as T
-- containers
import qualified Data.Map as M
import Data.Sequence (Seq, (|>), (<|))
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
import Data.Generics.Encode.Internal (Heidi, toVal, Val(..), header, Header(..),  VP(..))
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





-- | render the frame header
-- >>> printBox $ headerBox $ header (Proxy @R)

-- -- headerBox :: Header String -> Box
-- headerBox h0 = go h0 0
--   where
--     go h d =
--       case h of
--         HSum ty hm -> withHM '+' ty hm
--         HProd ty hm -> withHM '*' ty hm
--         HLeaf l -> HLeaf (text l, d) -- fixme depth to be adjusted with 'moveDown'
--       where
--         d' = d + 3 -- depth of deepest rendered layer so far
--         withHM sep ty hm = boxLayer
--           where
--             boxLayer = text ty /|/
--                        dashesWith sep bxs /|/
--                        hSepList bxs
--             bxs = values $ HM.mapWithKey (\k hrest -> text k /|/ go hrest d') hm


headerBox :: Show a => Header a -> Box
headerBox h =
  case h of
      HSum ty hm -> withHM '+' ty hm
      HProd ty hm -> withHM '*' ty hm
      HLeaf l -> text $ show l
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




-- examples

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
