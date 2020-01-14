-----------------------------------------------------------------------------
-- |
-- Module      :  Heidi
-- Description :  tidy data in Haskell
-- Copyright   :  (c) Marco Zocca (2018-2019)
-- License     :  BSD-style
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Heidi : tidy data in Haskell
-- 
-----------------------------------------------------------------------------
module Heidi (
  -- * Frame
  Frame
  -- ** Construction
  , fromList
  -- ** Access
  , head, take, drop, zipWith, numRows
  -- ** Filtering 
  , filter
  -- *** 'D.Decode'-based filtering
  , filterDecode
  -- ** Grouping
  , groupWith
  -- ** Scans 
  , scanl, scanr
  -- -- ** Data tidying
  -- , spread, gather
  -- -- ** Relational operations
  -- , groupBy, innerJoin, leftOuterJoin
  -- ** Vector-related
  , toVector, fromVector  
  -- * Encode
  , gToFrameHM, gToFrameGT, Heidi, TC, tcTyN, tcTyCon, mkTyN, mkTyCon, VP
  -- * Decode
  , D.Decode, D.runDecode
  -- ** Row
  -- *** HashMap
  
  -- *** generic-trie
  
  -- ** 'MonadThrow' getters
  , TypeError(..)
  ) where

import Control.Monad.Catch (MonadThrow(..))

import Core.Data.Frame.List (Frame, fromList, head, take, drop, zipWith, numRows, filter, filterA, groupWith, scanl, scanr, toVector, fromVector)
-- import Core.Data.Frame (Frame, fromNEList, fromList, head, take, drop, zipWith, numRows, filter, filterDecode, groupWith, scanl, scanr, toVector, fromVector)
import Core.Data.Frame.Generic (gToFrameHM, gToFrameGT)
import Data.Generics.Encode.Internal (Heidi, VP(..))
import qualified Data.Generics.Decode as D (Decode, runDecode)
import Data.Generics.Codec (TC(..), tcTyN, tcTyCon, mkTyN, mkTyCon, TypeError(..))


-- import Control.Monad.Catch (MonadThrow(..))
import Prelude hiding (filter, zipWith, lookup, foldl, foldr, scanl, scanr, head, take, drop)

-- | Filter a 'Frame' by decoding row values.
--
-- This is an intermediate function that doesn't require fixing the row type within the 'Frame'.
--
-- NB: a 'D.Decode' returning 'Bool' can be declared via its Functor, Applicative and Alternative instances.
filterDecode :: Applicative f =>
                D.Decode f row Bool   -- ^ Row decoder
             -> Frame row
             -> f (Frame row)
filterDecode dec = filterA (D.runDecode dec)
