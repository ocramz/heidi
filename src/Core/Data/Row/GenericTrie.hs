{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Core.Data.Row.GenericTrie
-- Description :  A sparse dataframe row, based on GenericTrie
-- Copyright   :  (c) Marco Zocca (2018-2019)
-- License     :  BSD-style
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Rows are internally represented with prefix trees ("tries"), as provided by the
-- @generic-trie@ library; in addition to supporting the possibility of missing features in the dataset, tries provide fast insertion and lookup functionality when keyed with structured datatypes (such as lists or trees).
--
-----------------------------------------------------------------------------
module Core.Data.Row.GenericTrie where

import qualified Data.GenericTrie as GT


-- | A 'Row' type is internally a Trie:
--
-- * Fast random access (logarithmic on average)
-- * Fast set operations 
-- * Supports missing elements 
newtype Row k v = Row { unRow :: GT.Trie k v } deriving (Functor, Foldable, Traversable)
instance (GT.TrieKey k, Show k, Show v) => Show (Row k v) where
  show = show . GT.toList . unRow
