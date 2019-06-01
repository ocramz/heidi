{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Heidi.Data.Row.GenericTrie
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
module Heidi.Data.Row.GenericTrie (
    Row
    -- * Construction
  , fromKVs, emptyRow
  -- ** (unsafe)  
  , mkRow
  -- * Update  
  , insert
  -- * Access  
  , toList, keys
  -- * Filtering
  , delete, filterWithKey, removeKnownKeys
  -- ** Decoders  
  , real, scientific, text, string, oneHot
  -- * Lookup  
  , lookup, lookupThrowM, elemSatisfies
  -- ** Lookup utilities
  , maybeEmpty
  -- ** Comparison by lookup
  , eqByLookup, eqByLookups
  , compareByLookup
  -- * Set operations  
  , union, unionWith
  -- * Traversals  
  , traverseWithKey
  ) where

import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Control.Applicative (Alternative(..))
import qualified Data.Foldable as F
-- import Control.Monad (filterM)
import Control.Monad.Catch(MonadThrow(..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Set as S (Set, member)

import qualified Data.GenericTrie as GT

import qualified Data.Generics.Decode as D (Decode, mkDecode)
import Data.Generics.Decode ((>>>))
import Data.Generics.Encode.Internal (VP)
import Data.Generics.Encode.OneHot (OneHot)
import Core.Data.Row.Decode
import Core.Data.Row.Internal (KeyError(..))

import Prelude hiding (lookup)


-- $setup
-- >>> import Data.Generics.Encode.Internal (VP)
-- >>> let row0 = fromKVs [(0, 'a'), (3, 'b')] :: Row Int Char
-- >>> let row1 = fromKVs [(0, 'x'), (1, 'b'), (666, 'z')] :: Row Int Char


-- | A 'Row' type is internally a Trie:
--
-- * Fast random access 
-- * Fast set operations 
-- * Supports missing elements 
newtype Row k v = Row { unRow :: GT.Trie k v } deriving (Functor, Foldable, Traversable)
instance (GT.TrieKey k, Show k, Show v) => Show (Row k v) where
  show = show . GT.toList . unRow

instance (GT.TrieKey k, Eq k, Eq v) => Eq (Row k v) where
  r1 == r2 = toList r1 == toList r2

instance (GT.TrieKey k, Eq k, Eq v, Ord k, Ord v) => Ord (Row k v) where
  r1 <= r2 = toList r1 <= toList r2

-- | Construct a 'Row' from a list of key-element pairs.
--
-- >>> lookup 3 (fromKVs [(3,'a'),(4,'b')])
-- Just 'a'
-- >>> lookup 6 (fromKVs [(3,'a'),(4,'b')])
-- Nothing
fromKVs :: GT.TrieKey k => [(k, v)] -> Row k v
fromKVs = Row . GT.fromList

-- | Construct a 'Row' from a trie (unsafe).
mkRow :: GT.Trie k v -> Row k v
mkRow = Row

-- | An empty row
emptyRow :: GT.TrieKey k => Row k v
emptyRow = Row GT.empty

-- | Access the key-value pairs contained in the 'Row'
toList :: GT.TrieKey k => Row k v -> [(k ,v)]
toList = GT.toList . unRow

-- | Lookup the value stored at a given key in a row
--
-- >>> lookup 0 row0
-- Just 'a'
-- >>> lookup 1 row0
-- Nothing
lookup :: (GT.TrieKey k) => k -> Row k v -> Maybe v
lookup k = GT.lookup k . unRow

liftLookup :: GT.TrieKey k =>
              (a -> b -> c) -> k -> Row k a -> Row k b -> Maybe c
liftLookup f k r1 r2 = f <$> lookup k r1 <*> lookup k r2


-- | Compares for ordering two rows by the values indexed at a specific key.
--
-- Returns Nothing if the key is not present in either row.
compareByLookup :: (GT.TrieKey k, Eq k, Ord a) =>
                   k -> Row k a -> Row k a -> Maybe Ordering
compareByLookup = liftLookup compare

-- | Compares two rows by the values indexed at a specific key.
--
-- Returns Nothing if the key is not present in either row.
eqByLookup :: (GT.TrieKey k, Eq k, Eq a) =>
              k -> Row k a -> Row k a -> Maybe Bool
eqByLookup = liftLookup (==)

-- | Compares two rows by the values indexed at a set of keys.
--
-- Returns Nothing if a key in either row is not present.
eqByLookups :: (Foldable t, GT.TrieKey k, Eq k, Eq a) =>
               t k -> Row k a -> Row k a -> Maybe Bool
eqByLookups ks r1 r2 = F.foldlM insf True ks where
  insf b k = (&&) <$> pure b <*> eqByLookup k r1 r2

-- | Like 'lookup', but throws a 'KeyError' if the lookup is unsuccessful
lookupThrowM :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k) =>
                k -> Row k v -> m v
lookupThrowM k r = maybe (throwM $ MissingKeyError k) pure (lookup k r)

-- | Returns an empty row if the argument is Nothing.
maybeEmpty :: GT.TrieKey k => Maybe (Row k v) -> Row k v
maybeEmpty = fromMaybe emptyRow

-- | List the keys of a given row
--
-- >>> keys row0
-- [0,3]
keys :: GT.TrieKey k => Row k v -> [k]
keys = map fst . toList

-- | Returns a new 'Row' that doesn't have a given key-value pair
delete :: GT.TrieKey k =>
          k       -- ^ Key to remove
       -> Row k v
       -> Row k v
delete k (Row gt) = Row $ GT.delete k gt


-- | Filter a row by applying a predicate to its keys and corresponding elements.
--
-- NB : filtering _retains_ the elements that satisfy the predicate.
filterWithKey :: GT.TrieKey k => (k -> v -> Bool) -> Row k v -> Row k v
filterWithKey ff (Row gt) = Row $ GT.filterWithKey ff gt

-- | Produce a new 'Row' such that its keys do _not_ belong to a certain set.
removeKnownKeys :: (GT.TrieKey k, Ord k) => S.Set k -> Row k v -> Row k v
removeKnownKeys ks = filterWithKey f where
  f k _ = not $ S.member k ks


-- alter k m = fromMaybe m $ do
--   v <- lookup k m 
--   delete k m

-- alter k f t =
--   case f (lookup k t) of
--     Just v' -> insert k v' t
--     Nothing -> delete k t


-- | Insert a key-value pair into a row and return the updated one
-- 
-- >>> keys $ insert 2 'y' row0
-- [0,2,3]
insert :: (GT.TrieKey k) => k -> v -> Row k v -> Row k v
insert k v = Row . GT.insert k v . unRow

-- | Traverse a 'Row' using a function of both the key and the element.
traverseWithKey :: (Applicative f, GT.TrieKey k) => (k -> a -> f b) -> Row k a -> f (Row k b)
traverseWithKey f r = Row <$> GT.traverseWithKey f (unRow r)


-- | Set union of two rows
--
-- >>> keys $ union row0 row1
-- [0,1,3,666]
union :: (GT.TrieKey k) => Row k v -> Row k v -> Row k v
union r1 r2 = Row $ GT.union (unRow r1) (unRow r2)

-- | Set union of two rows, using a combining function for equal keys
unionWith :: (GT.TrieKey k) =>
             (v -> v -> v) -> Row k v -> Row k v -> Row k v
unionWith f r1 r2 = Row $ GT.unionWith f (unRow r1) (unRow r2)



-- | Looks up a key from a row and applies a predicate to its value (if this is found). If no value is found at that key the function returns False.
--
-- This function is meant to be used as first argument to 'filter'.
--
-- >>> elemSatisfies (== 'a') 0 row0
-- True
-- >>> elemSatisfies (== 'a') 42 row0
-- False
elemSatisfies :: (GT.TrieKey k) => (a -> Bool) -> k -> Row k a -> Bool
elemSatisfies f k row = maybe False f (lookup k row)


-- | Lookup a value from a Row indexed at the given key (returns in a MonadThrow type)
lookupColM :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k) =>
              k -> D.Decode m (Row k o) o
lookupColM k = D.mkDecode (lookupThrowM k)

-- -- | Lookup a value from a Row indexed at the given key (returns in the Maybe monad)
-- lookupCol :: GT.TrieKey k => k -> D.Decode Maybe (Row k o) o
-- lookupCol k = D.mkDecode (lookup k)





-- * Decoders

-- | Lookup and decode a real number
real :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k, Alternative m) =>
        k -> D.Decode m (Row k VP) Double
real k = lookupColM k >>> decodeRealM

-- | Lookup and decode a real 'Scientific' value
scientific :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k, Alternative m) =>
              k -> D.Decode m (Row k VP) Scientific
scientific k = lookupColM k >>> decodeScientificM

-- | Lookup and decode a text string (defaults to Text)
text :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k, Alternative m) =>
        k -> D.Decode m (Row k VP) Text
text k = lookupColM k >>> decodeTextM

-- | Lookup and decode a text string (defaults to 'String')
string :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k, Alternative m) =>
          k -> D.Decode m (Row k VP) String
string k = lookupColM k >>> decodeStringM

-- | Lookup and decode a one-hot encoded enum
oneHot :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k) =>
          k -> D.Decode m (Row k VP) (OneHot Int)
oneHot k = lookupColM k >>> decOneHotM




-- spork k1 k2 = (>) <$> real k1 <*> real k2


