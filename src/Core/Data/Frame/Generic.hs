{-# language
    DataKinds
  , FlexibleContexts
  , GADTs 
  , LambdaCase 
  , TypeOperators
  , ScopedTypeVariables
#-}
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
import Control.Monad.Catch (MonadThrow(..))

-- import Core.Data.Frame (Frame, fromList)
import qualified Core.Data.Frame.List as FL (Frame, fromList)
import qualified Heidi.Data.Row.HashMap as HMR (Row, mkRow)
import qualified Heidi.Data.Row.GenericTrie as GTR (Row, mkRow)
import Data.Generics.Encode.Internal (gflattenHM, gflattenGT, HasGE, TC, VP)


-- $setup
-- >>> :set -XDeriveGeneric
-- >>> import qualified GHC.Generics as G
-- >>> import qualified Data.Generics.Encode.Internal as GE
-- >>> data P1 = P1 Int Char deriving (Eq, Show, G.Generic)
-- >>> instance GE.HasGE P1
-- >>> data P2 = P2 { p2i :: Int, p2c :: Char } deriving (Eq, Show, G.Generic)
-- >>> instance GE.HasGE P2
-- >>> data Q = Q (Maybe Int) (Either Double Char) deriving (Eq, Show, G.Generic)
-- >>> instance GE.HasGE Q

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
gToFrameHM :: (MonadThrow m, Foldable t, HasGE a) =>
              t a
           -> m (FL.Frame (HMR.Row [TC] VP))
gToFrameHM ds
  | null ds = throwM NoDataE 
  | otherwise = pure $ FL.fromList $ map gToRowHM $ F.toList ds


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
gToFrameGT :: (MonadThrow m, Foldable t, HasGE a) =>
              t a
           -> m (FL.Frame (GTR.Row [TC] VP))
gToFrameGT ds
  | null ds = throwM NoDataE 
  | otherwise = pure $ FL.fromList $ map gToRowGT $ F.toList ds  

-- | Populate a 'Row' with a generic encoding of the input value (hashmap backend)
gToRowHM :: HasGE a => a -> HMR.Row [TC] VP
gToRowHM = HMR.mkRow . gflattenHM

-- | Populate a 'Row' with a generic encoding of the input value (generic-trie backend)
gToRowGT :: HasGE a => a -> GTR.Row [TC] VP
gToRowGT = GTR.mkRow . gflattenGT

-- | Exceptions related to the input data
data DataException =
    -- AnonRecordE -- ^ Anonymous records not implemented yet
    NoDataE     -- ^ Dataset has 0 rows
  deriving (Eq, Typeable)
instance Show DataException where
  show = \case
    -- AnonRecordE -> "Anonymous records not implemented yet"
    NoDataE -> "The dataset has 0 rows"
instance Exception DataException

