{-# language
    DeriveGeneric
  , DeriveDataTypeable
  , FlexibleContexts
  , GADTs
  , OverloadedStrings
  , DefaultSignatures
  , ScopedTypeVariables
  , FlexibleInstances
  , LambdaCase
#-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- {-# OPTIONS_GHC -Wno-unused-top-binds #-}
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
                                      -- ** 'MonadThrow' getters
                                     getIntM, getBoolM, getFloatM, getDoubleM, getScientificM, getCharM, getStringM, getTextM, getOneHotM, TypeError(..),
                                     -- * TC (Type and Constructor annotation)
                                     TC(..), tcTyN, tcTyCon, 
                                     -- * HasGE (generic ADT encoding)
                                     HasGE) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Typeable (Typeable)
import qualified GHC.Generics as G
import Generics.SOP (All, DatatypeName, datatypeName, DatatypeInfo, FieldInfo(..), FieldName, ConstructorInfo(..), constructorInfo, All, All2, hcliftA2, hcmap, Proxy(..), SOP(..), NP(..), I(..), K(..), mapIK, hcollapse)
-- import Generics.SOP.NP (cpure_NP)
-- import Generics.SOP.Constraint (SListIN)
import Generics.SOP.GGP (GCode, GDatatypeInfo, GFrom, gdatatypeInfo, gfrom)

import Control.Monad.Catch(Exception(..), MonadThrow(..))
import Data.Hashable (Hashable(..))
-- import qualified Data.Text as T ()
import Data.Text (Text)
-- import Data.Time (Day, LocalTime, TimeOfDay)
-- import qualified Data.Vector as V
-- import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
-- import qualified Data.GenericTrie as GT
import Data.Scientific (Scientific)
import Data.Generics.Encode.OneHot (OneHot, mkOH)
-- import Data.List (unfoldr)
-- import qualified Data.Foldable as F
-- import qualified Data.Sequence as S (Seq(..), empty)
-- import Data.Sequence ((<|), (|>))
import qualified Data.GenericTrie as GT
import Prelude hiding (getChar)

-- $setup
-- >>> :set -XDeriveDataTypeable
-- >>> :set -XDeriveGeneric
-- >>> import Generics.SOP (Generic(..), All, Code)
-- >>> import Generics.SOP.NP
-- >>> import qualified GHC.Generics as G


-- | Flatten a value into a 1-layer hashmap, via the value's generic encoding
gflattenHM :: HasGE a => a -> HM.HashMap [TC] VP
gflattenHM = flattenHM . toVal

-- | Flatten a value into a 'GT.Trie', via the value's generic encoding
gflattenGT :: HasGE a => a -> GT.Trie [TC] VP
gflattenGT = flattenGT . toVal


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


-- | Primitive types
data VP =
    VPInt    Int
  | VPInt8   Int8
  | VPInt16   Int16
  | VPInt32   Int32
  | VPInt64   Int64
  | VPWord   Word  
  | VPWord8   Word8
  | VPWord16   Word16
  | VPWord32   Word32
  | VPWord64   Word64  
  | VPBool    Bool
  | VPFloat  Float
  | VPDouble Double
  | VPScientific Scientific
  | VPChar   Char
  | VPString String
  | VPText   Text
  | VPOH     (OneHot Int)  -- ^ A 1-hot encoding of an enum value
  deriving (Eq, Show, G.Generic)
instance Hashable VP

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
  deriving (Show, Eq, Typeable)
instance Exception TypeError 


-- | Internal representation of encoded ADTs values
-- 
-- The first String parameter contains the type name at the given level, the second contains the type constructor name
data Val =
    VRec   String        (HM.HashMap String Val) -- ^ recursion
  | VEnum  String String (OneHot Int)            -- ^ 1-hot encoding of an enum
  | VPrim  VP                                    -- ^ primitive types
  deriving (Eq, Show)


-- | NOTE: if your type has a 'G.Generic' instance you just need to declare an empty instance of 'HasGE' for it (a default implementation of 'toVal' is provided).
--
-- example:
--
-- @
-- data A = A Int Char deriving ('G.Generic')
-- instance 'HasGE' A
-- @
class HasGE a where
  toVal :: a -> Val
  default toVal ::
    (G.Generic a, All2 HasGE (GCode a), GFrom a, GDatatypeInfo a) => a -> Val
  toVal x = sopHasGE (gdatatypeInfo (Proxy :: Proxy a)) (gfrom x)  


sopHasGE :: All2 HasGE xss => DatatypeInfo xss -> SOP I xss -> Val
sopHasGE di sop@(SOP xss) = hcollapse $ hcliftA2
    (Proxy :: Proxy (All HasGE))
    (\ci xs -> K (mkVal ci xs tyName oneHot))
    (constructorInfo di)
    xss
  where
     tyName = datatypeName di
     oneHot = mkOH di sop
     
mkVal :: All HasGE xs =>
         ConstructorInfo xs -> NP I xs -> DatatypeName -> OneHot Int -> Val
mkVal cinfo xs tyn oh = case cinfo of
    Infix cn _ _  -> VRec cn $ mkAnonProd xs
    Constructor cn
      | null cns  -> VEnum tyn cn oh
      | otherwise -> VRec cn  $ mkAnonProd xs
    Record _ fi   -> VRec tyn $ mkProd fi xs
  where
    cns :: [Val]
    cns = npHasGEs xs

mkProd :: All HasGE xs => NP FieldInfo xs -> NP I xs -> HM.HashMap String Val
mkProd fi xs = HM.fromList $ hcollapse $ hcliftA2 (Proxy :: Proxy HasGE) mk fi xs where
  mk :: HasGE v => FieldInfo v -> I v -> K (FieldName, Val) v
  mk (FieldInfo n) (I x) = K (n, toVal x)

mkAnonProd :: All HasGE xs => NP I xs -> HM.HashMap String Val
mkAnonProd xs = HM.fromList $ zip labels cns where
  cns = npHasGEs xs

npHasGEs :: All HasGE xs => NP I xs -> [Val]
npHasGEs xs = hcollapse $ hcmap (Proxy :: Proxy HasGE) (mapIK toVal) xs

-- | >>> take 3 labels
-- ["_0","_1","_2"]
labels :: [String]
labels = map (('_' :) . show) [0 ..]


instance HasGE Bool where toVal = VPrim . VPBool
instance HasGE Int where toVal = VPrim . VPInt
instance HasGE Int8 where toVal = VPrim . VPInt8
instance HasGE Int16 where toVal = VPrim . VPInt16
instance HasGE Int32 where toVal = VPrim . VPInt32
instance HasGE Int64 where toVal = VPrim . VPInt64
instance HasGE Word8 where toVal = VPrim . VPWord8
instance HasGE Word16 where toVal = VPrim . VPWord16
instance HasGE Word32 where toVal = VPrim . VPWord32
instance HasGE Word64 where toVal = VPrim . VPWord64
instance HasGE Float where toVal = VPrim . VPFloat
instance HasGE Double where toVal = VPrim . VPDouble
instance HasGE Scientific where toVal = VPrim . VPScientific
instance HasGE Char where toVal = VPrim . VPChar
instance HasGE String where toVal = VPrim . VPString
instance HasGE Text where toVal = VPrim . VPText

instance HasGE a => HasGE (Maybe a) where
  toVal = \case
    Nothing -> VRec "Maybe" HM.empty
    Just x  -> VRec "Maybe" $ HM.singleton "Just" $ toVal x
  
instance (HasGE a, HasGE b) => HasGE (Either a b) where
  toVal = \case
    Left  l -> VRec "Either" $ HM.singleton "Left" $ toVal l
    Right r -> VRec "Either" $ HM.singleton "Right" $ toVal r

instance (HasGE a, HasGE b) => HasGE (a, b) where
  toVal (x, y) = VRec "(,)" $ HM.fromList $ zip labels [toVal x, toVal y]

instance (HasGE a, HasGE b, HasGE c) => HasGE (a, b, c) where
  toVal (x, y, z) = VRec "(,,)" $ HM.fromList $ zip labels [toVal x, toVal y, toVal z] 








-- -- examples

-- data A0 = A0 deriving (Eq, Show, G.Generic)
-- instance HasGE A0
-- newtype A = A Int deriving (Eq, Show, G.Generic)
-- instance HasGE A
-- newtype A2 = A2 { a2 :: Int } deriving (Eq, Show, G.Generic)
-- instance HasGE A2
-- data B = B Int Char deriving (Eq, Show, G.Generic)
-- instance HasGE B
-- data B2 = B2 { b21 :: Int, b22 :: Char } deriving (Eq, Show, G.Generic)
-- instance HasGE B2
-- data C = C1 | C2 | C3 deriving (Eq, Show, G.Generic)
-- instance HasGE C
-- data D = D (Maybe Int) (Either Int String) deriving (Eq, Show, G.Generic)
-- instance HasGE D
-- data E = E (Maybe Int) (Maybe Char) deriving (Eq, Show, G.Generic)
-- instance HasGE E
-- newtype F = F (Int, Char) deriving (Eq, Show, G.Generic)
-- instance HasGE F

