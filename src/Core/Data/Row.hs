{-# language TypeFamilies #-}
-- {-# language ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Core.Data.Row
-- Description :  A sparse dataframe row
-- Copyright   :  (c) Marco Zocca (2018-2019)
-- License     :  BSD-style
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
--
-----------------------------------------------------------------------------
module Core.Data.Row
  (
  Row, RowKey(..)
  -- ,
  -- -- * Construction
  -- fromKVs,
  -- -- ** (unsafe)
  -- mkRow, 
  -- -- * Update
  -- insert, insertRowFun, insertRowFunM, 
  -- -- * Access
  -- toList, keys, elems,
  -- -- ** Decoders
  -- real, scientific, text, oneHot, 
  -- -- * Lookup
  -- lookup, lookupThrowM, lookupDefault, (!:), elemSatisfies, 
  -- -- * Set operations
  -- union, unionWith,
  -- -- * Traversals
  -- traverseWithKey,
  -- -- * Key constraint
  -- Key,
  )
  where

import Data.Maybe (fromMaybe)
import qualified Data.Set as S 



class RowKey k where
  type RowRep k :: * -> *
  fromList :: [(k, v)] -> Row k v
  -- | Empty row
  empty :: Row k v
  -- | Is a row empty?
  null :: Row k v -> Bool
  -- | Lookup a column key within a row
  lookup :: k -> Row k v -> Maybe v
  -- | All keys of a row
  keys :: Row k v -> [k]  
  -- | All elements of a row
  elems :: Row k v -> [v]
  -- | Insert a value at a key
  insert :: k -> v -> Row k v -> Row k v
  -- | Construct a row holding a single value
  singleton :: k -> v -> Row k v
  -- | Apply a function to all elements of a row
  map :: (a -> b) -> Row k a -> Row k b
  -- | Traverse the values stored in a row
  rowTraverse :: Applicative f => (a -> f b) -> Row k a -> f (Row k b)
  -- | Filter a row using a predicate of both key and value
  filterWithKey :: (k -> v -> Bool) -> Row k v -> Row k v
  -- | Traverse a row with a function of both key and value.  
  traverseWithKey :: Applicative f => (k -> a -> f b) -> Row k a -> f (Row k b)
  -- | Union of two rows
  union :: Row k v -> Row k v -> Row k v
  -- | Union of two rows using a binary function of the values  
  unionWith :: (v -> v -> v) -> Row k v -> Row k v -> Row k v

newtype Row k v = MkRow (RowRep k v)


maybeEmpty :: RowKey k => Maybe (Row k v) -> Row k v
maybeEmpty = fromMaybe empty

removeKnownKeys :: (Ord k, RowKey k) => S.Set k -> Row k v -> Row k v 
removeKnownKeys ks = filterWithKey f where
  f k _ = not $ S.member k ks
