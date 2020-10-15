{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# language DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# options_ghc -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Data.Generics.Encode.Internal.Simple where

import Data.Proxy (Proxy)
import qualified GHC.Generics as G

-- containers
import qualified Data.Map as M (Map, fromList, insert, lookup)
-- generics-sop
import Generics.SOP (All, HasDatatypeInfo(..), datatypeInfo, DatatypeName, datatypeName, DatatypeInfo, FieldInfo(..), FieldName, fieldName, ConstructorInfo(..), constructorInfo, All, All2, hcliftA, hcliftA2, hcmap, Proxy(..), SOP(..), NP(..), I(..), K(..), unK, mapIK, hcollapse, SListI)
-- import Generics.SOP.NP (cpure_NP)
-- import Generics.SOP.Constraint (SListIN)
import Generics.SOP.GGP (GCode, GDatatypeInfo, GFrom, gdatatypeInfo, gfrom)
-- hashable
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM

import Data.Generics.Encode.Internal.Prim (VP(..))


-- -- examples

data A0 = A0 deriving (Eq, Show, G.Generic)
newtype A = A Int deriving (Eq, Show, G.Generic, HasHeader)
newtype A2 = A2 { a2 :: Int } deriving (Eq, Show, G.Generic, HasHeader)
data B = B Int Char deriving (Eq, Show, G.Generic, HasHeader)
data B2 = B2 { b21 :: Int, b22 :: Char } deriving (Eq, Show, G.Generic, HasHeader)
data C = C1 Int | C2 Char | C3 String deriving (Eq, Show, G.Generic, HasHeader)
data D = D (Maybe Int) (Either Int String) deriving (Eq, Show, G.Generic)
data E = E (Maybe Int) (Maybe Char) deriving (Eq, Show, G.Generic)
data R = R { r1 :: B2, r2 :: C } deriving (Eq, Show, G.Generic, HasHeader)

instance HasHeader Int where hasHeader _ = HPrim "Int"
instance HasHeader Char where hasHeader _ = HPrim "Char"
instance HasHeader () where hasHeader _ = HUnit
instance HasHeader a => HasHeader [a]

data Header =
     HProd [String] (HM.HashMap String Header) -- ^ products
   | HPrim String -- ^ primitive types
   | HUnit
   deriving (Eq, Show)

instance Semigroup Header where
  HProd a hma <> HProd _ hmb = HProd a $ HM.union hma hmb
instance Monoid Header where
  mempty = z

z :: Header
z = HProd [] mempty



-- λ> hasHeader (Proxy :: Proxy R)
-- HProd ["R"] (fromList [
--                 ("r1",HProd ["B2"] (fromList [
--                                        ("b21",HPrim "Int"),
--                                        ("b22",HPrim "Char")])),
--                 ("r2",HProd ["C"] (fromList [
--                                       ("C1",HUnit),
--                                       ("C3",HUnit),
--                                       ("C2",HUnit)]))])

class HasHeader a where
  hasHeader :: Proxy a -> Header
  default hasHeader ::
    (G.Generic a, All2 HasHeader (GCode a), GDatatypeInfo a) => Proxy a -> Header
  hasHeader _ = hasHeader' (gdatatypeInfo (Proxy :: Proxy a))


hasHeader' :: (All2 HasHeader xs, SListI xs) => DatatypeInfo xs -> Header
hasHeader' di = mconcat $ hcollapse $ hcliftA allp (goConstructor n) cinfo
  where
    cinfo = constructorInfo di
    n = datatypeName di

goConstructor :: (All HasHeader xs) => DatatypeName -> ConstructorInfo xs -> K Header xs
goConstructor dtn = \case
  Record n ns -> K $ HProd [n] (mkProd ns)
  Constructor n -> K $ HProd [dtn] (mkAnonProd n)


mkAnon :: forall a . (HasHeader a) => Proxy a -> HM.HashMap String Header
mkAnon px = HM.fromList $ hcollapse $ singletonK (anon px)
  where
    anon :: Proxy a -> K (String, Header) a
    anon _ = goFieldAnon "_"


mkAnonProd :: String -> HM.HashMap String Header
mkAnonProd cn = HM.fromList $ hcollapse $ singletonK anon
  where
    anon :: K (String, Header) () -- FIXME should not be () but type from context
    anon = goFieldAnon cn

singletonK :: K a x -> NP (K a) '[x]
singletonK x@(K _) = x :* Nil

mkProd :: All HasHeader xs => NP FieldInfo xs -> HM.HashMap String Header
mkProd finfo = HM.fromList $ hcollapse $ hcliftA p goField finfo

goField :: forall a . (HasHeader a) => FieldInfo a -> K (String, Header) a
goField (FieldInfo n) = goFieldAnon n -- K (n, hasHeader (Proxy :: Proxy a))

goFieldAnon :: forall a . HasHeader a => String -> K (String, Header) a
goFieldAnon n = K (n, hasHeader (Proxy :: Proxy a))

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
