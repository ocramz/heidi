{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# language TemplateHaskell #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.Generics.Encode.Internal.Prim (VP(..), vpInt, vpBool, vpFloat, vpDouble, vpScientific, vpChar, vpString, vpText, vpOneHot, vpDay, vpUTCTime, vpTimeOfDay, vpLocalTime, vpTimeZone, vpNominalDiffTime, vpDiffTime, vpUniversalTime) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified GHC.Generics as G

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

-- hashable-time
import Data.Hashable.Time (Hashable(..))

import Data.Generics.Encode.OneHot (OneHot, mkOH)

-- | Primitive types
--
-- NB : this is just a convenience for unityping the dataframe contents, but it should not be exposed to the library users 
data VP =
     VPInt    { _vpInt :: Int }    -- ^ 'Int'
   | VPInt8   Int8  -- ^ 'Int8'
   | VPInt16   Int16  -- ^ 'Int16'
   | VPInt32   Int32 -- ^ 'Int32'
   | VPInt64   Int64 -- ^ 'Int64'
   | VPWord   Word   -- ^ 'Word'
   | VPWord8   Word8  -- ^ 'Word8'
   | VPWord16   Word16 -- ^ 'Word16'
   | VPWord32   Word32 -- ^ 'Word32'
   | VPWord64   Word64   -- ^ 'Word64'
   | VPBool   { _vpBool :: Bool } -- ^ 'Bool'
   | VPFloat  { _vpFloat :: Float } -- ^ 'Float'
   | VPDouble { _vpDouble :: Double } -- ^ 'Double'
   | VPScientific { _vpScientific :: Scientific } -- ^ 'Scientific'
   | VPChar   { _vpChar :: Char } -- ^ 'Char'
   | VPString { _vpString :: String } -- ^ 'String'
   | VPText   { _vpText :: Text } -- ^ 'Text'
   | VPOH     { _vpOneHot :: OneHot Int }  -- ^ 1-hot encoding of an enum value
   | VPUnit
   | VPDay { _vpDay :: Day } -- ^ 'Day'
   | VPUTCTime { _vpUTCTime :: UTCTime } -- ^ 'UTCTime'
   | VPTimeOfDay { _vpTimeOfDay :: TimeOfDay } -- ^ 'TimeOfDay'
   | VPLocalTime { _vpLocalTime :: LocalTime } -- ^ 'LocalTime'
   | VPTimeZone { _vpTimeZone :: TimeZone } -- ^ 'TimeZone'
   | VPNominalDiffTime { _vpNominalDiffTime :: NominalDiffTime } -- ^ 'NominalDiffTime'
   | VPDiffTime { _vpDiffTime :: DiffTime } -- ^ 'DiffTime'
   | VPUniversalTime { _vpUniversalTime :: UniversalTime } -- ^ 'UniversalTime'
   
   

   deriving (Eq, Ord, G.Generic)
instance Hashable VP
makeLenses ''VP

instance Show VP where
  show = \case
    VPInt x -> show x
    VPInt8 x -> show x
    VPInt16 x -> show x
    VPInt32 x -> show x
    VPInt64 x -> show x
    VPWord x -> show x
    VPWord8 x -> show x
    VPWord16 x -> show x
    VPWord32 x -> show x
    VPWord64 x -> show x
    VPBool b   -> show b
    VPFloat f -> show f
    VPDouble d -> show d
    VPScientific s -> show s
    VPChar d -> pure d
    VPString s -> s
    VPText t -> unpack t
    VPOH oh -> show oh
    VPUnit -> show ()
    VPDay d -> show d
    VPUTCTime t -> show t
    VPTimeOfDay t -> show t
    VPLocalTime t -> show t
    VPTimeZone t -> show t
    VPNominalDiffTime t -> show t
    VPDiffTime t -> show t
    VPUniversalTime t -> show t
