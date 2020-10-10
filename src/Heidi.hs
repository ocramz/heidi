-----------------------------------------------------------------------------
-- |
-- Module      :  Heidi
-- Description :  tidy data in Haskell
-- Copyright   :  (c) Marco Zocca (2018-2020)
-- License     :  BSD-style
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Heidi : tidy data in Haskell
--
-- In Heidi, a data 'Frame' is not meant to be constructed directly, but 'encode'd from a
-- collection of values. The encoding produces a simple representation which can be easily manipulated for common data analysis tasks.
--
-- 
--
-----------------------------------------------------------------------------
{-# options_ghc -Wno-unused-imports #-}
module Heidi (
  -- * Frame
  Frame
  -- ** Construction
  -- *** Encoding
  , encode, Heidi, TC, VP
  -- *** Direct
  , frameFromList
  -- ** Access
  , head, take, drop, numRows
  -- ** Filtering
  , filter, filterA
  -- ** Grouping
  , groupWith
  -- ** Zipping
  , zipWith
  -- ** Scans
  , scanl, scanr
  -- * Data tidying
  , spreadWith, gatherWith
  -- * Relational operations
  , groupBy, innerJoin, leftOuterJoin
  -- ** Vector-related
  , toVector, fromVector

  -- * Row
  , Row
  -- * Construction
  , rowFromList
  -- ** Access
  , toList, keys
  -- * Filtering
  , delete, filterWithKey, filterWithKeyPrefix, filterWithKeyAny
  , deleteMany
  -- * Partitioning
  , partitionWithKey, partitionWithKeyPrefix
  -- -- ** Decoders
  -- , real, scientific, text, string, oneHot
  -- * Lookup
  , lookup
  -- , lookupThrowM
  , (!:), elemSatisfies
  -- ** Lookup utilities
  , maybeEmpty
  -- ** Comparison by lookup
  , eqByLookup, eqByLookups
  , compareByLookup
  -- * Set operations
  , union, unionWith
  , intersection, intersectionWith
  -- * Maps
  , mapWithKey
  -- * Folds
  , foldWithKey, keysOnly
  -- * Traversals
  , traverseWithKey
  -- * Lenses
  , int, bool, float, double, char, string, text, scientific, oneHot
  -- ** Lens combinators
  , at, keep
  -- *** Combinators for list-indexed rows
  , atPrefix, eachPrefixed, foldPrefixed
  -- ** Encode internals
  , tcTyN, tcTyCon, mkTyN, mkTyCon
  -- , DataException(..)
  , OneHot
  )
  where

import Control.Monad.Catch (MonadThrow(..))

import Core.Data.Frame.List (Frame, frameFromList, head, take, drop, zipWith, numRows, filter, filterA, groupWith, scanl, scanr, toVector, fromVector)
import Core.Data.Frame.Generic (encode)
import Data.Generics.Encode.Internal (Heidi, VP(..))
import Data.Generics.Encode.OneHot (OneHot)
import Data.Generics.Codec (TC(..), tcTyN, tcTyCon, mkTyN, mkTyCon, TypeError(..))
import Heidi.Data.Row.GenericTrie 
import Heidi.Data.Frame.Algorithms.GenericTrie (innerJoin, leftOuterJoin, gatherWith, spreadWith, groupBy)

-- import Control.Monad.Catch (MonadThrow(..))
import Prelude hiding (filter, zipWith, lookup, foldl, foldr, scanl, scanr, head, take, drop)

