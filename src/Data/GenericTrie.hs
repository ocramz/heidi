{-# LANGUAGE Safe #-}

-- Copyright (c) 2014, Eric Mertens
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Eric Mertens nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{- |

This module implements an interface for working with maps.

For primitive types, like 'Int', the library automatically selects
an efficient implementation (e.g., an "IntMap").

For complex structured types, the library uses an implementation
based on tries: this is useful when using large and similar keys where
comparing for order may become expensive, and storing the distinct
keys would be inefficient.

The 'OrdKey' type allows for maps with complex keys,
where the keys are compared based on order, rather than using the
trie implementation.

All methods of 'TrieKey' can be derived automatically using
a 'GHC.Generics.Generic' instance.

@
data Demo = DemoC1 'Int' | DemoC2 'Int' 'Char'  deriving 'GHC.Generics.Generic'

instance 'TrieKey' Demo
@

-}

module Data.GenericTrie
  (
  -- * Trie interface
    Trie
  , TrieKey
  , ShowTrieKey

  -- ** Construction
  , empty
  , singleton
  , fromList
  , fromListWith
  , fromListWith'

  -- ** Updates
  , alter
  , insert
  , insertWith
  , insertWith'
  , delete
  , at

  -- ** Queries
  , member
  , notMember
  , null
  , lookup

  -- ** Folding
  , foldWithKey
  , fold
  , toList

  -- ** Traversing
  , traverseWithKey
  , traverseMaybeWithKey
  , mapMaybe
  , mapMaybeWithKey
  , filter
  , filterWithKey

  -- ** Combining maps
  , union
  , unionWith
  , unionWithKey
  , intersection
  , intersectionWith
  , intersectionWithKey
  , difference
  , differenceWith
  , differenceWithKey

  -- * Keys using 'Ord'
  , OrdKey(..)
  ) where

import Control.Applicative (Applicative)
import Data.List (foldl')
import Data.Maybe (isNothing, isJust)
import Prelude hiding (lookup, null, filter)

import Data.GenericTrie.Internal

------------------------------------------------------------------------------
-- Various helpers
------------------------------------------------------------------------------

-- | Construct a trie from a list of key-value pairs
fromList :: TrieKey k => [(k,v)] -> Trie k v
fromList = foldl' (\acc (k,v) -> insert k v acc) empty

-- | Construct a trie from a list of key-value pairs.
-- The given function is used to combine values at the
-- same key.
fromListWith :: TrieKey k => (v -> v -> v) -> [(k,v)] -> Trie k v
fromListWith f = foldl' (\acc (k,v) -> insertWith f k v acc) empty

-- | Version of 'fromListWith' which is strict in the result of
-- the combining function.
fromListWith' :: TrieKey k => (v -> v -> v) -> [(k,v)] -> Trie k v
fromListWith' f = foldl' (\acc (k,v) -> insertWith' f k v acc) empty

-- | Construct an empty trie
empty :: TrieKey k => Trie k a
empty = trieEmpty
{-# INLINE empty #-}

-- | Test for an empty trie
null :: TrieKey k => Trie k a -> Bool
null = trieNull
{-# INLINE null #-}

-- | Lookup an element from a trie
lookup :: TrieKey k => k -> Trie k a -> Maybe a
lookup = trieLookup
{-# INLINE lookup #-}

-- | Lens for the value at a given key
at :: (Functor f, TrieKey k) => k -> (Maybe a -> f (Maybe a)) -> Trie k a -> f (Trie k a)
at k f m = fmap aux (f mv)
  where
  mv = lookup k m
  aux r = case r of
    Nothing -> maybe m (const (delete k m)) mv
    Just v' -> insert k v' m

-- | Insert an element into a trie
insert :: TrieKey k => k -> a -> Trie k a -> Trie k a
insert = trieInsert
{-# INLINE insert #-}

-- | Delete an element from a trie
delete :: TrieKey k => k -> Trie k a -> Trie k a
delete = trieDelete
{-# INLINE delete #-}

-- | Construct a trie holding a single value
singleton :: TrieKey k => k -> a -> Trie k a
singleton = trieSingleton
{-# INLINE singleton #-}

-- | Apply a function to the values of a trie and keep the elements
-- of the trie that result in a 'Just' value.
mapMaybeWithKey :: TrieKey k => (k -> a -> Maybe b) -> Trie k a -> Trie k b
mapMaybeWithKey = trieMapMaybeWithKey
{-# INLINE mapMaybeWithKey #-}

-- | Perform an action for each value in a trie and keep the elements
-- of the trie that result in a 'Just' value.
traverseMaybeWithKey :: (TrieKey k, Applicative f)
                     => (k -> a -> f (Maybe b)) -> Trie k a -> f (Trie k b)
traverseMaybeWithKey = trieTraverseMaybeWithKey
{-# INLINE traverseMaybeWithKey #-}

-- | Filter the values of a trie with the given predicate.
filter :: TrieKey k => (a -> Bool) -> Trie k a -> Trie k a
filter p = filterWithKey (const p)

-- | Version of 'filter' where the predicate also gets the key.
filterWithKey :: TrieKey k => (k -> a -> Bool) -> Trie k a -> Trie k a
filterWithKey p = mapMaybeWithKey aux
  where
  aux k x
    | p k x     = Just x
    | otherwise = Nothing


-- | Fold a trie with a function of the value
fold :: TrieKey k => (a -> r -> r) -> r -> Trie k a -> r
fold = trieFoldWithKey . const
{-# INLINE fold #-}

-- | Fold a trie with a function of both key and value
foldWithKey :: TrieKey k => (k -> a -> r -> r) -> r -> Trie k a -> r
foldWithKey = trieFoldWithKey
{-# INLINE foldWithKey #-}

-- | Traverse a trie with a function of both key and value
traverseWithKey :: (TrieKey k, Applicative f) => (k -> a -> f b) -> Trie k a -> f (Trie k b)
traverseWithKey = trieTraverseWithKey
{-# INLINE traverseWithKey #-}

mergeWithKey ::
  TrieKey k =>
  (k -> a -> b -> Maybe c) ->
  (Trie k a -> Trie k c) ->
  (Trie k b -> Trie k c) ->
  Trie k a -> Trie k b -> Trie k c
mergeWithKey = trieMergeWithKey
{-# INLINE mergeWithKey #-}

-- | Alter the value at the given key location.
-- The parameter function takes the value stored
-- at the given key, if one exists, and should return a value to insert at
-- that location, or 'Nothing' to delete from that location.
alter :: TrieKey k => k -> (Maybe a -> Maybe a) -> Trie k a -> Trie k a
alter k f t =
  case f (lookup k t) of
    Just v' -> insert k v' t
    Nothing -> delete k t

-- | Insert a value at the given key. The combining function is used
-- when a value is already stored at that key. The new value is the
-- first argument to the combining function.
insertWith :: TrieKey k => (v -> v -> v) -> k -> v -> Trie k v -> Trie k v
insertWith f k v = alter k $ \mb ->
                      case mb of
                        Just v0 -> Just (f v v0)
                        Nothing -> Just v

-- | Version of 'insertWith' that is strict in the result of combining
-- two elements.
insertWith' :: TrieKey k => (v -> v -> v) -> k -> v -> Trie k v -> Trie k v
insertWith' f k v = alter k $ \mb ->
                      case mb of
                        Just v0 -> Just $! f v v0
                        Nothing -> Just v

-- | Returns 'True' when the 'Trie' has a value stored at the given key.
member :: TrieKey k => k -> Trie k a -> Bool
member k t = isJust (lookup k t)

-- | Returns 'False' when the 'Trie' has a value stored at the given key.
notMember :: TrieKey k => k -> Trie k a -> Bool
notMember k t = isNothing (lookup k t)

-- | Transform a trie to an association list.
toList :: TrieKey k => Trie k a -> [(k,a)]
toList = foldWithKey (\k v xs -> (k,v) : xs) []

-- | Left-biased union of two tries
union :: TrieKey k => Trie k a -> Trie k a -> Trie k a
union = mergeWithKey (\_ a _ -> Just a) id id

-- | Union of two tries with function used to merge overlapping elements
unionWith :: TrieKey k => (a -> a -> a) -> Trie k a -> Trie k a -> Trie k a
unionWith f = mergeWithKey (\_ a b -> Just (f a b)) id id

-- | Union of two tries with function used to merge overlapping elements along with key
unionWithKey :: TrieKey k => (k -> a -> a -> a) -> Trie k a -> Trie k a -> Trie k a
unionWithKey f = mergeWithKey (\k a b -> Just (f k a b)) id id

-- | Left-biased intersection of two tries
intersection :: TrieKey k => Trie k a -> Trie k b -> Trie k a
intersection = mergeWithKey (\_ a _ -> Just a) (const empty) (const empty)

-- | Intersection of two tries parameterized by a combining function of the
-- values at overlapping keys
intersectionWith :: TrieKey k => (a -> b -> c) -> Trie k a -> Trie k b -> Trie k c
intersectionWith f = mergeWithKey (\_ a b -> Just (f a b)) (const empty) (const empty)

-- | Intersection of two tries parameterized by a combining function of the
-- key and the values at overlapping keys
intersectionWithKey :: TrieKey k => (k -> a -> b -> c) -> Trie k a -> Trie k b -> Trie k c
intersectionWithKey f = mergeWithKey (\k a b -> Just (f k a b)) (const empty) (const empty)

-- | Remove the keys of the right trie from the left trie
difference :: TrieKey k => Trie k a -> Trie k b -> Trie k a
difference = mergeWithKey (\_ _ _ -> Nothing) id (const empty)

-- | Parameterized 'difference' using a custom merge function.
-- Return 'Just' to change the value stored in left trie, or
-- 'Nothing' to remove from the left trie.
differenceWith :: TrieKey k => (a -> b -> Maybe a) -> Trie k a -> Trie k b -> Trie k a
differenceWith f = mergeWithKey (\_ -> f) id (const empty)

-- | 'differenceWith' where function also has access to the key
differenceWithKey :: TrieKey k => (k -> a -> b -> Maybe a) -> Trie k a -> Trie k b -> Trie k a
differenceWithKey f = mergeWithKey f id (const empty)

-- | Map a function over a trie filtering out elements where function returns 'Nothing'
mapMaybe :: TrieKey k => (a -> Maybe b) -> Trie k a -> Trie k b
mapMaybe f = mapMaybeWithKey (\_ -> f)
