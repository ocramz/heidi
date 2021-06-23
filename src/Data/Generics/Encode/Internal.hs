{-# LANGUAGE DeriveTraversable #-}


{-# language
    DeriveGeneric
  , DeriveAnyClass
  , DeriveDataTypeable
  , FlexibleContexts
  , GADTs
  , OverloadedStrings
  , DefaultSignatures
  , ScopedTypeVariables
  , FlexibleInstances
  , LambdaCase
  , TemplateHaskell
  , TypeApplications
#-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# options_ghc -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Encode.Internal
-- Description :  Generic encoding of algebraic datatypes
-- Copyright   :  (c) Marco Zocca (2019)
-- License     :  MIT
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Generic encoding of algebraic datatypes, using @generics-sop@
--
-- Examples, inspiration and code borrowed from :
-- 
-- * @basic-sop@ - generic show function : https://hackage.haskell.org/package/basic-sop-0.2.0.2/docs/src/Generics-SOP-Show.html#gshow
-- 
-- * @tree-diff@ - single-typed ADT reconstruction : http://hackage.haskell.org/package/tree-diff-0.0.2/docs/src/Data.TreeDiff.Class.html#sopToExpr
-----------------------------------------------------------------------------
module Data.Generics.Encode.Internal (gflattenHM, gflattenGT,
                                      -- * VP (Primitive types)
                                      VP(..),
                                      -- ** Lenses
                                      vpInt, vpDouble, vpFloat, vpString, vpText, vpBool, vpScientific, vpChar, vpOneHot, vpDay, vpUTCTime, vpTimeOfDay, vpLocalTime, vpTimeZone, vpNominalDiffTime, vpDiffTime, vpUniversalTime,
                                      -- ** 'MonadThrow' getters
                                      getIntM, getInt8M, getInt16M, getInt32M, getInt64M, getWordM, getWord8M, getWord16M, getWord32M, getWord64M, getBoolM, getFloatM, getDoubleM, getScientificM, getCharM, getStringM, getTextM, getOneHotM, getDayM, getUTCTimeM, getTimeOfDayM, getLocalTimeM, getTimeZoneM, getNominalDiffTimeM, getDiffTimeM, getUniversalTimeM, TypeError(..),
                                     -- * TC (Type and Constructor annotation)
                                     TC(..), tcTyN, tcTyCon, mkTyN, mkTyCon,
                                     -- * Heidi (generic ADT encoding)
                                     Heidi, toVal, Val(..), header, Header(..)) where

import qualified GHC.Generics as G
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Proxy
import Data.Typeable (Typeable)

-- containers
import qualified Data.Map as M (Map, fromList, insert, lookup)
import Data.Sequence (Seq, (|>), (<|))
-- exceptions
import Control.Monad.Catch(Exception(..), MonadThrow(..))
-- generics-sop
import Generics.SOP (All, HasDatatypeInfo(..), datatypeInfo, DatatypeName, datatypeName, DatatypeInfo, FieldInfo(..), SListI, FieldName, ConstructorInfo(..), constructorInfo, All, All2, AllN, Prod, HAp, hcpure, hmap, hcliftA, hcliftA2, hcmap, Proxy(..), SOP(..), NP(..), I(..), K(..), mapIK, hcollapse)
-- import Generics.SOP.NP (cpure_NP)
-- import Generics.SOP.Constraint (SListIN)
import Generics.SOP.GGP (GCode, GDatatypeInfo, GFrom, gdatatypeInfo, gfrom)
-- generic-trie
import qualified Data.GenericTrie as GT
-- hashable
import Data.Hashable (Hashable(..))
-- microlens-th
import Lens.Micro.TH (makeLenses)
-- scientific
import Data.Scientific (Scientific)
-- text
import Data.Text (Text, unpack)
-- time
import Data.Time (Day, UTCTime,  TimeOfDay, LocalTime, TimeZone, NominalDiffTime, DiffTime, UniversalTime)

-- import Data.Time (Day, LocalTime, TimeOfDay)
-- import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
-- import qualified Data.GenericTrie as GT

import Data.Generics.Encode.OneHot (OneHot, mkOH)
import Data.Generics.Encode.Internal.Prim (VP(..), vpInt, vpBool, vpFloat, vpDouble, vpScientific, vpChar, vpString, vpText, vpOneHot, vpDay, vpUTCTime, vpTimeOfDay, vpLocalTime, vpTimeZone, vpNominalDiffTime, vpDiffTime, vpUniversalTime) 
-- import Data.List (unfoldr)
-- import qualified Data.Foldable as F
-- import qualified Data.Sequence as S (Seq(..), empty)
-- import Data.Sequence ((<|), (|>))

import Prelude hiding (getChar)

-- $setup
-- >>> :set -XDeriveGeneric
-- >>> import qualified GHC.Generics as G




-- | Flatten a value into a 1-layer hashmap, via the value's generic encoding
gflattenHM :: Heidi a => a -> HM.HashMap [TC] VP
gflattenHM = flattenHM . toVal

-- | Flatten a value into a 'GT.Trie', via the value's generic encoding
gflattenGT :: Heidi a => a -> GT.Trie [TC] VP
gflattenGT = flattenGT . toVal


-- -- | Commands for manipulating lists of TC's
-- data TCAlg = TCAnyTyCon String -- ^ Matches any type constructor name
--            | TCFirstTyCon String -- ^ " first type constructor name
--            | TCAnyTyN String -- ^ " any type name
--            | TCFirstTyN String -- ^ first type name


-- | A (type, constructor) name pair
data TC = TC String String deriving (Eq, Show, Ord, G.Generic)
instance Hashable TC
instance GT.TrieKey TC

-- | Type name
tcTyN :: TC -> String
tcTyN (TC n _) = n
-- | Type constructor
tcTyCon :: TC -> String
tcTyCon (TC _ c) = c

-- | Create a fake TC with the given string as type constructor
mkTyCon :: String -> TC
mkTyCon = TC ""

-- | Create a fake TC with the given string as type name
mkTyN :: String -> TC
mkTyN x = TC x ""

-- | Fold a 'Val' into a 1-layer hashmap indexed by the input value's (type, constructor) metadata
flattenHM :: Val -> HM.HashMap [TC] VP
flattenHM = flatten HM.empty HM.insert

-- | Fold a 'Val' into a 1-layer 'GT.Trie' indexed by the input value's (type, constructor) metadata
flattenGT :: Val -> GT.Trie [TC] VP
flattenGT = flatten GT.empty GT.insert

flatten :: t -> ([TC] -> VP -> t -> t) -> Val -> t
flatten z insf = go ([], z) where
  insRev ks = insf (reverse ks)
  go (ks, hmacc) = \case
    VRec ty hm     -> HM.foldlWithKey' (\hm' k t -> go (TC ty k : ks, hm') t) hmacc hm
    VEnum ty cn oh -> insRev (TC ty cn : ks) (VPOH oh) hmacc
    VPrim vp       -> insRev ks vp hmacc


-- | Collect the hashmap keys at the leaves. The resulting lists at the leaf nodes can be used to look up values in the rows
collectKeys :: Header String -> Header (Seq String)
collectKeys = go mempty
  where
    go acc = \case
      HLeaf x -> HLeaf (acc |> x)
      HSum ty hm -> HSum ty $ HM.mapWithKey (\k v -> go (acc |> k) v) hm
      HProd ty hm -> HProd ty $ HM.mapWithKey (\k v -> go (acc |> k)  v) hm


-- | Internal representation of encoded ADTs values
--
-- The first String parameter contains the type name at the given level, the second contains the type constructor name
data Val =
     VRec   String        (HM.HashMap String Val) -- ^ recursion
   | VEnum  String String (OneHot Int)            -- ^ 1-hot encoding of an enum
   | VPrim  VP                                    -- ^ primitive types
   deriving (Eq, Show)

-- the type param 't' can store information at the leaves, e.g. list-shaped keys for lookup
data Header t =
     HSum String (HM.HashMap String (Header t)) -- ^ sums
   | HProd String (HM.HashMap String (Header t)) -- ^ products
   | HLeaf t -- ^ primitive types
   deriving (Eq, Show, Functor, Foldable, Traversable)

instance Semigroup (Header t) where
  HProd s1 hm1 <> HProd s2 hm2 = HProd (s1 <> s2) (hm1 `HM.union` hm2) -- FIXME
instance Monoid t => Monoid (Header t) where
  mempty = HLeaf mempty

-- | Single interface to the library.
--
-- This typeclass provides all the machinery for encoding Haskell values into dataframes.
--
-- NOTE: Your datatypes only need to possess a 'G.Generic' instance, to which you just need to add an empty instance of 'Heidi'.
--
-- example:
--
-- @
-- {-\# language DeriveGenerics, DeriveAnyClass \#-}
--
-- data A = A Int Char deriving ('G.Generic', 'Heidi')
-- @
class Heidi a where
  toVal :: a -> Val
  default toVal ::
    (G.Generic a, All2 Heidi (GCode a), GFrom a, GDatatypeInfo a) => a -> Val
  toVal x = toVal' (gdatatypeInfo (Proxy :: Proxy a)) (gfrom x)
  header :: Proxy a -> Header String
  default header ::
    (G.Generic a, All2 Heidi (GCode a), GDatatypeInfo a) => Proxy a -> Header String
  header _ = header' (gdatatypeInfo (Proxy :: Proxy a))

toVal' :: All2 Heidi xss => DatatypeInfo xss -> SOP I xss -> Val
toVal' di sop@(SOP xss) = hcollapse $ hcliftA2
    allp
    (\ci xs -> K (mkVal ci xs tyName oneHot))
    (constructorInfo di)
    xss
  where
     tyName = datatypeName di
     oneHot = mkOH di sop

mkVal :: All Heidi xs =>
         ConstructorInfo xs
      -> NP I xs
      -> DatatypeName -> OneHot Int -> Val
mkVal cinfo xs tyn oh = case cinfo of
    Infix cn _ _  -> VRec cn $ mkAnonProd xs
    Constructor cn
      | null cns  -> VEnum tyn cn oh
      | otherwise -> VRec cn  $ mkAnonProd xs
    Record _ fi   -> VRec tyn $ mkProd fi xs
  where
    cns :: [Val]
    cns = npToVals xs

    mkProd :: All Heidi xs => NP FieldInfo xs -> NP I xs -> HM.HashMap String Val
    mkProd finfo xss = HM.fromList $
      hcollapse $ hcliftA2 p mk finfo xss
      where
        mk :: Heidi v => FieldInfo v -> I v -> K (FieldName, Val) v
        mk (FieldInfo n) (I x) = K (n, toVal x)

    mkAnonProd :: All Heidi xs => NP I xs -> HM.HashMap String Val
    mkAnonProd xss = HM.fromList $ zip labels cnss
      where
        cnss = npToVals xss

npToVals :: All Heidi xs => NP I xs -> [Val]
npToVals xs = hcollapse $ hcmap p (mapIK toVal) xs


header' :: (All2 Heidi xs, SListI xs) => DatatypeInfo xs -> Header String
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

goConstructor :: forall xs . (All Heidi xs) => String -> ConstructorInfo xs -> K (String, Header String) xs
goConstructor dtn = \case
  Record n ns -> K (n, mkProdH dtn ns)
  Constructor n -> K (n, mkAnonProdH dtn (Proxy @xs) )
  Infix n _ _ -> K (n, mkAnonProdH dtn (Proxy @xs) )


-- | anonymous products
mkAnonProdH ::
  forall xs. (SListI xs, All Heidi xs) => String -> Proxy xs -> Header String
mkAnonProdH dtn _  | single hs =
                    let hdr = head hs
                    in hdr
                  | null hs = HLeaf dtn
                  | otherwise = HProd dtn $ HM.fromList $ zip labels hs
  where
    hs :: [Header String]
    hs = hcollapse (hcpure p headerK :: NP (K (Header String)) xs)
    headerK :: forall a. Heidi a => K (Header String) a
    headerK = K (header (Proxy @a))

-- | products
mkProdH :: All Heidi xs => String -> NP FieldInfo xs -> Header String
mkProdH dtn finfo | single hs =
                    let (n, hdr) = head hs   -- FIXME why n is unused ?!
                    in hdr
                 | otherwise = HProd dtn $ HM.fromList hs
  where
    hs :: [(String, Header String)]
    hs = hcollapse $ hcliftA p goFieldH finfo

goFieldH :: forall a . (Heidi a) => FieldInfo a -> K (String, Header String) a
goFieldH (FieldInfo n) = goFieldAnonH n

goFieldAnonH :: forall a . Heidi a => String -> K (String, Header String) a
goFieldAnonH n = K (n, header (Proxy @a))

single :: [a] -> Bool
single = (== 1) . length


allp :: Proxy (All Heidi)
allp = Proxy

p :: Proxy Heidi
p = Proxy

-- | >>> take 3 labels
-- ["_0","_1","_2"]
labels :: [String]
labels = map (('_' :) . show) [0 ..]


instance Heidi () where {toVal _ = VPrim VPUnit ; header _ = HLeaf "()"}
instance Heidi Bool where toVal = VPrim . VPBool
instance Heidi Int where {toVal = VPrim . VPInt ; header _ = HLeaf "Int"}
instance Heidi Int8 where {toVal = VPrim . VPInt8 ; header _ = HLeaf "Int8"}
instance Heidi Int16 where {toVal = VPrim . VPInt16 ; header _ = HLeaf "Int16"}
instance Heidi Int32 where {toVal = VPrim . VPInt32 ; header _ = HLeaf "Int32"}
instance Heidi Int64 where {toVal = VPrim . VPInt64 ; header _ = HLeaf "Int64"}
instance Heidi Word where {toVal = VPrim . VPWord ; header _ = HLeaf "Word"}
instance Heidi Word8 where {toVal = VPrim . VPWord8 ; header _ = HLeaf "Word8"}
instance Heidi Word16 where {toVal = VPrim . VPWord16 ; header _ = HLeaf "Word16"}
instance Heidi Word32 where {toVal = VPrim . VPWord32 ; header _ = HLeaf "Word32"}
instance Heidi Word64 where {toVal = VPrim . VPWord64 ; header _ = HLeaf "Word64"}
instance Heidi Float where {toVal = VPrim . VPFloat ; header _ = HLeaf "Float"}
instance Heidi Double where {toVal = VPrim . VPDouble ; header _ = HLeaf "Double"}
instance Heidi Scientific where {toVal = VPrim . VPScientific ; header _ = HLeaf "Scientific"}
instance Heidi Char where {toVal = VPrim . VPChar ; header _ = HLeaf "Char"}
instance Heidi String where {toVal = VPrim . VPString ; header _ = HLeaf "String"}
instance Heidi Text where {toVal = VPrim . VPText ; header _ = HLeaf "Text"}
instance Heidi Day where {toVal = VPrim . VPDay ; header _ = HLeaf "Day"}
instance Heidi UTCTime where {toVal = VPrim . VPUTCTime ; header _ = HLeaf "UTCTime"}
instance Heidi TimeOfDay where {toVal = VPrim . VPTimeOfDay ; header _ = HLeaf "TimeOfDay"}
instance Heidi LocalTime where {toVal = VPrim . VPLocalTime ; header _ = HLeaf "LocalTime"}
instance Heidi TimeZone where {toVal = VPrim . VPTimeZone ; header _ = HLeaf "TimeZone"}
instance Heidi NominalDiffTime where {toVal = VPrim . VPNominalDiffTime ; header _ = HLeaf "NominalDiffTime"}
instance Heidi DiffTime where {toVal = VPrim . VPDiffTime ; header _ = HLeaf "DiffTime"}
instance Heidi UniversalTime where {toVal = VPrim . VPUniversalTime ; header _ = HLeaf "UniversalTime"}


instance Heidi a => Heidi (Maybe a) where
  toVal = \case
    Nothing -> VRec "Maybe" HM.empty
    Just x  -> VRec "Maybe" $ HM.singleton "Just" $ toVal x
  header _ = HSum "Maybe" $ HM.fromList [
    ("Nothing", HLeaf "_") ,
    ("Just", header (Proxy @a))]

instance (Heidi a, Heidi b) => Heidi (Either a b) where
  toVal = \case
    Left  l -> VRec "Either" $ HM.singleton "Left" $ toVal l
    Right r -> VRec "Either" $ HM.singleton "Right" $ toVal r

instance (Heidi a, Heidi b) => Heidi (a, b) where
  toVal (x, y) = VRec "(,)" $ HM.fromList $ zip labels [toVal x, toVal y]

instance (Heidi a, Heidi b, Heidi c) => Heidi (a, b, c) where
  toVal (x, y, z) = VRec "(,,)" $ HM.fromList $ zip labels [toVal x, toVal y, toVal z]





-- | Extract an Int
getInt :: VP -> Maybe Int
getInt = \case {VPInt i -> Just i; _ -> Nothing}
-- | Extract an Int8
getInt8 :: VP -> Maybe Int8
getInt8 = \case {VPInt8 i -> Just i; _ -> Nothing}
-- | Extract an Int16
getInt16 :: VP -> Maybe Int16
getInt16 = \case {VPInt16 i -> Just i; _ -> Nothing}
-- | Extract an Int32
getInt32 :: VP -> Maybe Int32
getInt32 = \case {VPInt32 i -> Just i; _ -> Nothing}
-- | Extract an Int64
getInt64 :: VP -> Maybe Int64
getInt64 = \case {VPInt64 i -> Just i; _ -> Nothing}
-- | Extract a Word
getWord :: VP -> Maybe Word
getWord = \case {VPWord i -> Just i; _ -> Nothing}
-- | Extract a Word8
getWord8 :: VP -> Maybe Word8
getWord8 = \case {VPWord8 i -> Just i; _ -> Nothing}
-- | Extract a Word16
getWord16 :: VP -> Maybe Word16
getWord16 = \case {VPWord16 i -> Just i; _ -> Nothing}
-- | Extract a Word32
getWord32 :: VP -> Maybe Word32
getWord32 = \case {VPWord32 i -> Just i; _ -> Nothing}
-- | Extract a Word64
getWord64 :: VP -> Maybe Word64
getWord64 = \case {VPWord64 i -> Just i; _ -> Nothing}
-- | Extract a Bool
getBool :: VP -> Maybe Bool
getBool = \case {VPBool i -> Just i; _ -> Nothing}
-- | Extract a Float
getFloat :: VP -> Maybe Float
getFloat = \case {VPFloat i -> Just i; _ -> Nothing}
-- | Extract a Double
getDouble :: VP -> Maybe Double
getDouble = \case {VPDouble i -> Just i; _ -> Nothing}
-- | Extract a Scientific
getScientific :: VP -> Maybe Scientific
getScientific = \case {VPScientific i -> Just i; _ -> Nothing}
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
getOneHot :: VP -> Maybe (OneHot Int)
getOneHot = \case {VPOH i -> Just i; _ -> Nothing}
-- | Extract a Day
getDay :: VP -> Maybe Day
getDay = \case {VPDay i -> Just i; _ -> Nothing}
-- | Extract a UTCTime
getUTCTime :: VP -> Maybe UTCTime
getUTCTime = \case {VPUTCTime i -> Just i; _ -> Nothing}
-- | Extract a TimeOfDay
getTimeOfDay :: VP -> Maybe TimeOfDay
getTimeOfDay = \case {VPTimeOfDay i -> Just i; _ -> Nothing}
-- | Extract a LocalTime
getLocalTime :: VP -> Maybe LocalTime
getLocalTime = \case {VPLocalTime i -> Just i; _ -> Nothing}
-- | Extract a TimeZone
getTimeZone :: VP -> Maybe TimeZone
getTimeZone = \case {VPTimeZone i -> Just i; _ -> Nothing}
-- | Extract a NominalDiffTime
getNominalDiffTime :: VP -> Maybe NominalDiffTime
getNominalDiffTime = \case {VPNominalDiffTime i -> Just i; _ -> Nothing}
-- | Extract a DiffTime
getDiffTime :: VP -> Maybe DiffTime
getDiffTime = \case {VPDiffTime i -> Just i; _ -> Nothing}
-- | Extract a UniversalTime
getUniversalTime :: VP -> Maybe UniversalTime
getUniversalTime = \case {VPUniversalTime i -> Just i; _ -> Nothing}
-- | Helper function for decoding into a 'MonadThrow'.
decodeM :: (MonadThrow m, Exception e) =>
           e -> (a -> m b) -> Maybe a -> m b
decodeM e = maybe (throwM e)


getIntM :: MonadThrow m => VP -> m Int
getIntM x = decodeM IntCastE pure (getInt x)
getInt8M :: MonadThrow m => VP -> m Int8
getInt8M x = decodeM Int8CastE pure (getInt8 x)
getInt16M :: MonadThrow m => VP -> m Int16
getInt16M x = decodeM Int16CastE pure (getInt16 x)
getInt32M :: MonadThrow m => VP -> m Int32
getInt32M x = decodeM Int32CastE pure (getInt32 x)
getInt64M :: MonadThrow m => VP -> m Int64
getInt64M x = decodeM Int64CastE pure (getInt64 x)
getWordM :: MonadThrow m => VP -> m Word
getWordM x = decodeM WordCastE pure (getWord x)
getWord8M :: MonadThrow m => VP -> m Word8
getWord8M x = decodeM Word8CastE pure (getWord8 x)
getWord16M :: MonadThrow m => VP -> m Word16
getWord16M x = decodeM Word16CastE pure (getWord16 x)
getWord32M :: MonadThrow m => VP -> m Word32
getWord32M x = decodeM Word32CastE pure (getWord32 x)
getWord64M :: MonadThrow m => VP -> m Word64
getWord64M x = decodeM Word64CastE pure (getWord64 x)
getBoolM :: MonadThrow m => VP -> m Bool
getBoolM x = decodeM BoolCastE pure (getBool x)
getFloatM :: MonadThrow m => VP -> m Float
getFloatM x = decodeM FloatCastE pure (getFloat x)
getDoubleM :: MonadThrow m => VP -> m Double
getDoubleM x = decodeM DoubleCastE pure (getDouble x)
getScientificM :: MonadThrow m => VP -> m Scientific
getScientificM x = decodeM ScientificCastE pure (getScientific x)
getCharM :: MonadThrow m => VP -> m Char
getCharM x = decodeM CharCastE pure (getChar x)
getStringM :: MonadThrow m => VP -> m String
getStringM x = decodeM StringCastE pure (getString x)
getTextM :: MonadThrow m => VP -> m Text
getTextM x = decodeM TextCastE pure (getText x)
getOneHotM :: MonadThrow m => VP -> m (OneHot Int)
getOneHotM x = decodeM OneHotCastE pure (getOneHot x)
getDayM :: MonadThrow m => VP -> m Day
getDayM x = decodeM DayCastE pure (getDay x)
getUTCTimeM :: MonadThrow m => VP -> m UTCTime
getUTCTimeM x = decodeM UTCTimeCastE pure (getUTCTime x)
getLocalTimeM :: MonadThrow m => VP -> m LocalTime
getLocalTimeM x = decodeM LocalTimeCastE pure (getLocalTime x)
getTimeOfDayM :: MonadThrow m => VP -> m TimeOfDay
getTimeOfDayM x = decodeM TimeOfDayCastE pure (getTimeOfDay x)
getTimeZoneM :: MonadThrow m => VP -> m TimeZone
getTimeZoneM x = decodeM TimeZoneCastE pure (getTimeZone x)
getNominalDiffTimeM :: MonadThrow m => VP -> m NominalDiffTime
getNominalDiffTimeM x = decodeM NominalDiffTimeCastE pure (getNominalDiffTime x)
getDiffTimeM :: MonadThrow m => VP -> m DiffTime
getDiffTimeM x = decodeM DiffTimeCastE pure (getDiffTime x)
getUniversalTimeM :: MonadThrow m => VP -> m UniversalTime
getUniversalTimeM x = decodeM UniversalTimeCastE pure (getUniversalTime x)

-- | Type errors
data TypeError =
    FloatCastE
  | DoubleCastE
  | ScientificCastE
  | IntCastE
  | Int8CastE
  | Int16CastE
  | Int32CastE
  | Int64CastE
  | WordCastE
  | Word8CastE
  | Word16CastE
  | Word32CastE
  | Word64CastE
  | BoolCastE
  | CharCastE
  | StringCastE
  | TextCastE
  | OneHotCastE
  | DayCastE
  | UTCTimeCastE
  | LocalTimeCastE
  | TimeOfDayCastE
  | NominalDiffTimeCastE
  | TimeZoneCastE
  | DiffTimeCastE
  | UniversalTimeCastE
  deriving (Show, Eq, Typeable)
instance Exception TypeError






-- -- examples

data A0 = MkA0 deriving (Eq, Show, G.Generic, Heidi)
data A = MkA Int deriving (Eq, Show, G.Generic, Heidi)
newtype A' = MkA' Int deriving (Eq, Show, G.Generic, Heidi)
newtype A2 = A2 { a2 :: Int } deriving (Eq, Show, G.Generic, Heidi)
data B = MkB Int Char deriving (Eq, Show, G.Generic, Heidi)
data B2 = MkB2 { b21 :: Int, b22 :: Char } deriving (Eq, Show, G.Generic, Heidi)
data C = MkC1 {c1 :: Int} | MkC2 A | MkC3 () deriving (Eq, Show, G.Generic, Heidi)
data C2 = C21 {c21a :: Int, c21b :: ()} | C22 {c22 :: A} | C23 () deriving (Eq, Show, G.Generic, Heidi)
data C3 = C31 | C32 | C33 deriving (Eq, Show, G.Generic, Heidi)
-- data C3 a = C31 a a deriving (Eq, Show, G.Generic, Heidi)
data D = D (Maybe Int) C deriving (Eq, Show, G.Generic, Heidi)
data E = E (Maybe Int) (Maybe Char) deriving (Eq, Show, G.Generic, Heidi)
data R = MkR { r1 :: B2, r2 :: C , r3 :: B } deriving (Eq, Show, G.Generic, Heidi)

-- newtype F = F (Int, Char) deriving (Eq, Show, G.Generic)
-- instance Heidi F

