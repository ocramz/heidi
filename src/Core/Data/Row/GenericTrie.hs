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
module Core.Data.Row.GenericTrie (
    Row
    -- * Construction
  , fromKVs
  -- ** (unsafe)  
  , mkRow
  -- * Update  
  , insert
  -- * Access  
  , toList, keys
  -- ** Decoders  
  , real, scientific, text, oneHot
  -- * Lookup  
  , lookup, lookupThrowM, elemSatisfies
  -- * Set operations  
  , union, unionWith
  -- * Traversals  
  , traverseWithKey
  ) where

import Data.Typeable (Typeable)
import Control.Applicative (Alternative(..))
-- import Control.Monad (filterM)
import Control.Monad.Catch(MonadThrow(..))
import Data.Scientific (Scientific)
import Data.Text (Text)

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
-- * Fast random access (logarithmic on average)
-- * Fast set operations 
-- * Supports missing elements 
newtype Row k v = Row { unRow :: GT.Trie k v } deriving (Functor, Foldable, Traversable)
instance (GT.TrieKey k, Show k, Show v) => Show (Row k v) where
  show = show . GT.toList . unRow

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

-- | Like 'lookup', but throws a 'MissingKeyError' if the lookup is unsuccessful
lookupThrowM :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k) =>
                k -> Row k v -> m v
lookupThrowM k r = maybe (throwM $ MissingKeyError k) pure (lookup k r)

-- | List the keys of a given row
--
-- >>> keys row0
-- [0,3]
keys :: GT.TrieKey k => Row k v -> [k]
keys = map fst . toList 


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

-- | Lookup and decode a text string
text :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k, Alternative m) =>
        k -> D.Decode m (Row k VP) Text
text k = lookupColM k >>> decodeTextM

-- | Lookup and decode a one-hot encoded enum
oneHot :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k) =>
          k -> D.Decode m (Row k VP) (OneHot Int)
oneHot k = lookupColM k >>> decOneHotM




-- spork k1 k2 = (>) <$> real k1 <*> real k2


