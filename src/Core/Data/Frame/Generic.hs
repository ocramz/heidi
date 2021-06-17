{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -Wall #-}
{-# options_ghc -Wno-unused-imports #-}
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
    encode,
    -- -- * Exceptions
    -- DataException(..)
  ) where

import qualified Data.Foldable as F (toList)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Control.Exception (Exception(..))
import GHC.Generics (Generic(..))
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- microlens
import Lens.Micro (toListOf)

import qualified Core.Data.Frame.List as FL (Frame(..))
import qualified Heidi.Data.Row.GenericTrie as GTR (Row, mkRow)
import Data.Generics.Encode.Internal (gflattenHM, gflattenGT, Heidi, header, TC(..), VP)


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


-- | Populate a 'Frame' with the generic encoding of the row data
--
-- For example, a list of records having two fields each will produce a dataframe with two columns, having the record field names as column labels.
--
-- @
-- data P1 = P1 Int Char deriving (Eq, Show, 'G.Generic')
-- instance 'Heidi' P1
--
-- data P2 = P2 { p2i :: Int, p2c :: Char } deriving (Eq, Show, Generic)
-- instance Heidi P2
--
-- data Q = Q (Maybe Int) (Either Double Char) deriving (Eq, Show, Generic)
-- instance Heidi Q
-- @
--
-- >>> encode [P1 42 'z']
-- Frame {tableRows = [([TC "P1" "_0"],VPInt 42),([TC "P1" "_1"],VPChar 'z')] :| []}
--
-- >>> encode [P2 42 'z']
-- Frame {tableRows = [([TC "P2" "p2c"],VPChar 'z'),([TC "P2" "p2i"],VPInt 42)] :| []}
--
-- Test using 'Maybe' and 'Either' record fields :
--
-- >>> encode [Q (Just 42) (Left 1.2), Q Nothing (Right 'b')]
-- Frame {tableRows = [([TC "Q" "_0",TC "Maybe" "Just"],VPInt 42),([TC "Q" "_1",TC "Either" "Left"],VPDouble 1.2)] :| [[([TC "Q" "_1",TC "Either" "Right"],VPChar 'b')]]}
--
-- NB: as the last example above demonstrates, 'Nothing' values are not inserted in the rows, which can be used to encode missing data features.
encode :: (Foldable t, Heidi a) =>
          t a
       -> FL.Frame (GTR.Row [TC] VP)
encode ds = gToFrameWith gToRowGT ds

-- | Populate a 'Row' with a generic encoding of the input value (generic-trie backend)
gToRowGT :: Heidi a => a -> GTR.Row [TC] VP
gToRowGT = GTR.mkRow . gflattenGT

gToFrameWith :: forall a t row . (Heidi a, Foldable t) => (a -> row) -> t a -> FL.Frame row
gToFrameWith f = FL.Frame h . map f . F.toList
  where
    h = header (Proxy :: Proxy a)

-- -- | Exceptions related to the input data
-- data DataException =
--     NoDataE     -- ^ Dataset has 0 rows
--   deriving (Eq, Typeable)
-- instance Show DataException where
--   show = \case
--     NoDataE -> "The dataset has 0 rows"
-- instance Exception DataException






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
