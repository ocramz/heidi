{-# language DeriveGeneric #-}
{-# language LambdaCase #-}

{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Core.Data.Frame.Generic
-- Description :  Populate dataframes with generically-encoded data
-- Copyright   :  (c) Marco Zocca (2019)
-- License     :  MIT
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Generic encoding of algebraic datatypes, using 'generics-sop'
--
-----------------------------------------------------------------------------
module Core.Data.Frame.Generic (
    -- * HashMap-based rows 
    gToRowHM, gToFrameHM,
    -- * GenericTrie-based rows
    gToRowGT, gToFrameGT, 
    -- * Exceptions
    DataException(..)
  ) where

import qualified Data.Foldable as F (toList)
import Data.Typeable (Typeable)
import Control.Exception (Exception(..))
import GHC.Generics (Generic(..))
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- microlens
import Lens.Micro (toListOf)

import qualified Core.Data.Frame.List as FL (Frame, fromList)
import qualified Heidi.Data.Row.HashMap as HMR (Row, mkRow)
import qualified Heidi.Data.Row.GenericTrie as GTR (Row, mkRow)
import Data.Generics.Encode.Internal (gflattenHM, gflattenGT, Heidi(..), TC(..), VP)


-- $setup
-- >>> :set -XDeriveGeneric
-- >>> import qualified GHC.Generics as G
-- >>> import qualified Data.Generics.Encode.Internal as GE
-- >>> data P1 = P1 Int Char deriving (Eq, Show, G.Generic)
-- >>> instance GE.Heidi P1
-- >>> data P2 = P2 { p2i :: Int, p2c :: Char } deriving (Eq, Show, G.Generic)
-- >>> instance GE.Heidi P2
-- >>> data Q = Q (Maybe Int) (Either Double Char) deriving (Eq, Show, G.Generic)
-- >>> instance GE.Heidi Q

-- | Populate a 'Frame' with the generic encoding of the row data and throws a 'DataException' if the input data is malformed.
--
-- For example, a list of records having two fields each will produce a dataframe with two columns, having the record field names as column labels.
--
-- @
-- data P1 = P1 Int Char deriving (Eq, Show, 'G.Generic')
-- instance 'GE.Heidi' P1
-- 
-- data P2 = P2 { p2i :: Int, p2c :: Char } deriving (Eq, Show, Generic)
-- instance Heidi P2
--
-- data Q = Q (Maybe Int) (Either Double Char) deriving (Eq, Show, Generic)
-- instance Heidi Q
-- @
--
-- >>> gToFrameHM [P1 42 'z']
-- Frame {tableRows = [([TC "P1" "_1"],VPChar 'z'),([TC "P1" "_0"],VPInt 42)] :| []}
-- 
-- >>> gToFrameHM [P2 42 'z']
-- Frame {tableRows = [([TC "P2" "p2c"],VPChar 'z'),([TC "P2" "p2i"],VPInt 42)] :| []}
--
-- Test using 'Maybe' and 'Either' record fields :
--
-- >>> gToFrameHM [Q (Just 42) (Left 1.2), Q Nothing (Right 'b')]
-- Frame {tableRows = [([TC "Q" "_1",TC "Either" "Left"],VPDouble 1.2),([TC "Q" "_0",TC "Maybe" "Just"],VPInt 42)] :| [[([TC "Q" "_1",TC "Either" "Right"],VPChar 'b')]]}
--
-- NB: as the last example above demonstrates, 'Nothing' values are not inserted in the rows, which can be used to encode missing data features.
gToFrameHM :: (MonadThrow m, Foldable t, Heidi a) =>
              t a
           -> m (FL.Frame (HMR.Row [TC] VP))
gToFrameHM ds
  | null ds = throwM NoDataE
  | otherwise = pure $ gToFrameWith gToRowHM ds


-- | Populate a 'Frame' with the generic encoding of the row data and throws a 'DataException' if the input data is malformed.
--
-- For example, a list of records having two fields each will produce a dataframe with two columns, having the record field names as column labels.
--
-- @
-- data P1 = P1 Int Char deriving (Eq, Show, 'G.Generic')
-- instance 'GE.HasGE' P1
-- 
-- data P2 = P2 { p2i :: Int, p2c :: Char } deriving (Eq, Show, Generic)
-- instance HasGE P2
--
-- data Q = Q (Maybe Int) (Either Double Char) deriving (Eq, Show, Generic)
-- instance HasGE Q
-- @
--
-- >>> gToFrameGT [P1 42 'z']
-- Frame {tableRows = [([TC "P1" "_0"],VPInt 42),([TC "P1" "_1"],VPChar 'z')] :| []}
-- 
-- >>> gToFrameGT [P2 42 'z']
-- Frame {tableRows = [([TC "P2" "p2c"],VPChar 'z'),([TC "P2" "p2i"],VPInt 42)] :| []}
--
-- Test using 'Maybe' and 'Either' record fields :
--
-- >>> gToFrameGT [Q (Just 42) (Left 1.2), Q Nothing (Right 'b')]
-- Frame {tableRows = [([TC "Q" "_0",TC "Maybe" "Just"],VPInt 42),([TC "Q" "_1",TC "Either" "Left"],VPDouble 1.2)] :| [[([TC "Q" "_1",TC "Either" "Right"],VPChar 'b')]]}
--
-- NB: as the last example above demonstrates, 'Nothing' values are not inserted in the rows, which can be used to encode missing data features.
gToFrameGT :: (MonadThrow m, Foldable t, Heidi a) =>
              t a
           -> m (FL.Frame (GTR.Row [TC] VP))
gToFrameGT ds
  | null ds = throwM NoDataE
  | otherwise = pure $ gToFrameWith gToRowGT ds

-- | Populate a 'Row' with a generic encoding of the input value (hashmap backend)
gToRowHM :: Heidi a => a -> HMR.Row [TC] VP
gToRowHM = HMR.mkRow . gflattenHM

-- | Populate a 'Row' with a generic encoding of the input value (generic-trie backend)
gToRowGT :: Heidi a => a -> GTR.Row [TC] VP
gToRowGT = GTR.mkRow . gflattenGT

gToFrameWith :: Foldable t => (a -> row) -> t a -> FL.Frame row
gToFrameWith f = FL.fromList . map f . F.toList

-- | Exceptions related to the input data
data DataException =
    NoDataE     -- ^ Dataset has 0 rows
  deriving (Eq, Typeable)
instance Show DataException where
  show = \case
    NoDataE -> "The dataset has 0 rows"
instance Exception DataException






-- example data

{-
λ> gToRowGT $ B (A 42 'z') "moo"
[
 ([TC "B" "b1" ,TC "A" "a1" ], 42)
,([TC "B" "b1" ,TC "A" "a2"],  z)
,([TC "B" "b2"],               moo)
]
-}



data A = A { a1 :: Int, a2 :: Char } deriving (Eq, Show, Generic)
instance Heidi A

data B = B { b1 :: A, b2 :: String } deriving (Eq, Show, Generic)
instance Heidi B
