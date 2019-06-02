{-# language DeriveFunctor, GeneralizedNewtypeDeriving, DeriveTraversable #-}
-- {-# language ConstraintKinds #-}
-- {-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Heidi.Data.Row.HashMap
-- Description :  A sparse dataframe row, based on HashMap
-- Copyright   :  (c) Marco Zocca (2018-2019)
-- License     :  BSD-style
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Rows are internally represented with hash maps, as provided by the
-- @unordered-containers@ library; this format supports the possibility of missing features
-- in the dataset.
--
-----------------------------------------------------------------------------
module Heidi.Data.Row.HashMap (
  Row
  -- * Construction
  , fromList, emptyRow
  -- ** (unsafe)
  , mkRow
  -- * Update
  , insert, insertRowFun, insertRowFunM
  -- * Access
  , toList, keys, elems
  -- * Filtering
  , filterWithKey, removeKnownKeys
  -- ** Decoders
  , real, scientific, text, string, oneHot
  -- * Lookup
  , lookup, lookupThrowM, lookupDefault, (!:), elemSatisfies
  -- ** Lookup utilities
  , maybeEmpty  
  -- ** Comparison by lookup
  , eqByLookup, eqByLookups
  , compareByLookup
  -- * Set operations
  , union, unionWith
  , intersection, intersectionWith
  -- * Folds
  , foldlWithKey', foldrWithKey
  -- * Traversals
  , traverseWithKey
  -- * Key constraint
  , Key
    ) where

-- import Control.Monad (filterM)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Control.Applicative (Alternative(..))
import qualified Data.Foldable as F
import Data.Hashable (Hashable(..))
import Control.Monad.Catch(MonadThrow(..))
import qualified Data.HashMap.Strict as HM
import Data.Scientific (Scientific)
-- import qualified Data.Text as T (pack)
import Data.Text (Text)
import qualified Data.Set as S (Set, member)

import qualified Data.Generics.Decode as D (Decode, mkDecode)
import Data.Generics.Decode ((>>>))
import Data.Generics.Encode.Internal (VP)
import Data.Generics.Encode.OneHot (OneHot)
import Core.Data.Row.Decode
import Core.Data.Row.Internal (KeyError(..))

import Prelude hiding (lookup)



-- $setup
-- >>> let row0 = fromList [(0, 'a'), (3, 'b')] :: Row Int Char
-- >>> let row1 = fromList [(0, 'x'), (1, 'b'), (666, 'z')] :: Row Int Char

-- | A 'Row' type is internally a hashmap:
--
-- * Fast random access 
-- * Fast set operations 
-- * Supports missing elements 
newtype Row k v = Row { unRow :: HM.HashMap k v } deriving (Eq, Functor, Foldable, Traversable)

instance (Show k, Show v) => Show (Row k v) where
  show = show . HM.toList . unRow
  
instance (Ord k, Ord v) => Ord (Row k v) where
  r1 <= r2 = toList r1 <= toList r2  

-- | Construct a 'Row' from a list of key-element pairs.
--
-- >>> lookup 3 (fromList [(3,'a'),(4,'b')])
-- Just 'a'
-- >>> lookup 6 (fromList [(3,'a'),(4,'b')])
-- Nothing
fromList :: (Eq k, Hashable k) => [(k, v)] -> Row k v
fromList = Row . HM.fromList

-- | Wrap a HashMap into a Row constructor.
--
-- NB : This function is for internal use only. Do not use this function in application code, since it may break invariants such as key uniqueness.
mkRow :: HM.HashMap k v -> Row k v
mkRow = Row

-- | An empty row
emptyRow :: Row k v
emptyRow = Row HM.empty

-- | Access the key-value pairs contained in the 'Row'
toList :: Row k v -> [(k, v)]
toList = HM.toList . unRow

-- | Lookup the value stored at a given key in a row
--
-- >>> lookup 0 row0
-- Just 'a'
-- >>> lookup 1 row0
-- Nothing
lookup :: (Eq k, Hashable k) => k -> Row k v -> Maybe v
lookup k = HM.lookup k . unRow


liftLookup :: (Eq k, Hashable k) =>
              (a -> b -> c) -> k -> Row k a -> Row k b -> Maybe c
liftLookup f k r1 r2 = f <$> lookup k r1 <*> lookup k r2

-- | Compares for ordering two rows by the values indexed at a specific key.
--
-- Returns Nothing if the key is not present in either row.
compareByLookup :: (Hashable k, Eq k, Ord a) =>
                   k -> Row k a -> Row k a -> Maybe Ordering
compareByLookup = liftLookup compare

-- | Compares for equality two rows by the values indexed at a specific key.
--
-- Returns Nothing if the key is not present in either row.
eqByLookup :: (Hashable k, Eq k, Eq a) =>
              k -> Row k a -> Row k a -> Maybe Bool
eqByLookup = liftLookup (==)

-- | Compares two rows by the values indexed at a set of keys.
--
-- Returns Nothing if a key in either row is not present.
eqByLookups :: (Foldable t, Hashable k, Eq k, Eq a) =>
               t k -> Row k a -> Row k a -> Maybe Bool
eqByLookups ks r1 r2 = F.foldlM insf True ks where
  insf b k = (&&) <$> pure b <*> eqByLookup k r1 r2



-- lookupWith :: (Eq k, Hashable k) => (x -> k) -> x -> Row k v -> Maybe v
-- lookupWith f k = lookup (f k)

-- | Like 'lookup', but throws a 'KeyError' if the lookup is unsuccessful
lookupThrowM :: (MonadThrow m, Key k) =>
                k -> Row k v -> m v
lookupThrowM k r = maybe (throwM $ MissingKeyError k) pure (lookup k r)

-- | Returns an empty row if the argument is Nothing.
maybeEmpty :: Maybe (Row k v) -> Row k v
maybeEmpty = fromMaybe emptyRow


-- | A 'Key' must be 'Eq', 'Hashable', 'Show', 'Typeable'
class (Eq k, Hashable k, Show k, Typeable k) => Key k 


-- | Lookup a key using a default value for non-existing keys
--
-- >>> lookupDefault 'x' 0 row0
-- 'a'
-- >>> lookupDefault 'x' 2 row0
-- 'x'
lookupDefault :: (Eq k, Hashable k) => v -> k -> Row k v -> v
lookupDefault v k = HM.lookupDefault v k . unRow








-- | Insert a key-value pair into a row and return the updated one
-- 
-- >>> keys $ insert 2 'y' row0
-- [0,2,3]
insert :: (Eq k, Hashable k) => k -> v -> Row k v -> Row k v
insert k v = Row . HM.insert k v . unRow

-- | List the keys of a given row
--
-- >>> keys row0
-- [0,3]
keys :: Row k v -> [k]
keys = HM.keys . unRow

-- | List the elements of a given row
--
-- >>> elems row0
-- "ab"
elems :: Row k v -> [v]
elems = HM.elems . unRow

-- | Filter a row by applying a predicate to its keys and corresponding elements.
--
-- NB : filtering _retains_ the elements that satisfy the predicate.
filterWithKey :: (k -> v -> Bool) -> Row k v -> Row k v
filterWithKey ff (Row hm) = Row $ HM.filterWithKey ff hm

-- | Produce a new 'Row' such that its keys do _not_ belong to a certain set.
removeKnownKeys :: Ord k => S.Set k -> Row k v -> Row k v
removeKnownKeys ks = filterWithKey f where
  f k _ = not $ S.member k ks

-- | Left-associative fold over a row with a function of both key and value
foldlWithKey' :: (a -> k -> v -> a) -> a -> Row k v -> a
foldlWithKey' fk z (Row gt) = HM.foldlWithKey' fk z gt

-- | Right-associative fold over a row with a function of both key and value
foldrWithKey :: (k -> v -> a -> a) -> a -> Row k v -> a
foldrWithKey fk z (Row gt) = HM.foldrWithKey fk z gt

-- | Traverse a 'Row' using a function of both the key and the element.
traverseWithKey :: Applicative f => (k -> a -> f b) -> Row k a -> f (Row k b)
traverseWithKey f r = Row <$> HM.traverseWithKey f (unRow r)

-- | Set union of two rows
--
-- >>> keys $ union row0 row1
-- [0,1,3,666]
union :: (Eq k, Hashable k) => Row k v -> Row k v -> Row k v
union r1 r2 = Row $ HM.union (unRow r1) (unRow r2)

-- | Set union of two rows, using a combining function for equal keys
unionWith :: (Eq k, Hashable k) =>
             (v -> v -> v) -> Row k v -> Row k v -> Row k v
unionWith f r1 r2 = Row $ HM.unionWith f (unRow r1) (unRow r2)

-- | Set intersection of two rows
intersection :: (Eq k, Hashable k) => Row k v -> Row k b -> Row k v
intersection r1 r2 = Row $ HM.intersection (unRow r1) (unRow r2)

-- | Set intersections of two rows, using a combining function for equal keys
intersectionWith :: (Eq k, Hashable k) => (a -> b -> v) -> Row k a -> Row k b -> Row k v
intersectionWith f r1 r2 = Row $ HM.intersectionWith f (unRow r1) (unRow r2)


-- | Looks up a key from a row and applies a predicate to its value (if this is found). If no value is found at that key the function returns False.
--
-- This function is meant to be used as first argument to 'filter'.
--
-- >>> elemSatisfies (== 'a') 0 row0
-- True
-- >>> elemSatisfies (== 'a') 42 row0
-- False
elemSatisfies :: (Eq k, Hashable k) => (a -> Bool) -> k -> Row k a -> Bool
elemSatisfies f k row = maybe False f (lookup k row)






-- | Inline synonim for 'elemSatisfies'
(!:) :: (Eq k, Hashable k) => k -> (a -> Bool) -> Row k a -> Bool
k !: f = elemSatisfies f k 

-- | Updates a column with a function of the whole row
insertRowFun :: (Eq k, Hashable k) => (Row k v -> v) -> k -> Row k v -> Row k v
insertRowFun f knew row = insert knew (f row) row

-- | Monadic version of 'insertRowFun'
insertRowFunM :: (Eq k, Hashable k, Monad m) => (Row k v -> m v) -> k -> Row k v -> m (Row k v)
insertRowFunM fm knew row = do
  y <- fm row
  pure $ insert knew y row





-- | Lookup a value from a Row indexed at the given key (returns in a MonadThrow type)
lookupColM :: (MonadThrow m, Key k) =>
              k -> D.Decode m (Row k o) o
lookupColM k = D.mkDecode (lookupThrowM k)

-- -- | Lookup a value from a Row indexed at the given key (returns in the Maybe monad)
-- lookupCol :: (Eq k, Hashable k) => k -> D.Decode Maybe (Row k o) o
-- lookupCol k = D.mkDecode (lookup k)









-- | Lookup and decode a real number
real :: (Key k, MonadThrow m, Alternative m) => k -> D.Decode m (Row k VP) Double
real k = lookupColM k >>> decodeRealM

-- | Lookup and decode a real 'Scientific' value
scientific :: (Key k, MonadThrow m, Alternative m) => k -> D.Decode m (Row k VP) Scientific
scientific k = lookupColM k >>> decodeScientificM

-- | Lookup and decode a text string (defaults to 'Text')
text :: (Key k, MonadThrow m, Alternative m) => k -> D.Decode m (Row k VP) Text
text k = lookupColM k >>> decodeTextM

-- | Lookup and decode a text string (defaults to 'String')
string :: (MonadThrow m, Key k, Alternative m) =>
          k -> D.Decode m (Row k VP) String
string k = lookupColM k >>> decodeStringM

-- | Lookup and decode a one-hot encoded enum
oneHot :: (Key k, MonadThrow m) =>
          k -> D.Decode m (Row k VP) (OneHot Int)
oneHot k = lookupColM k >>> decOneHotM

-- -- example
-- sumCols :: (Key k, MonadThrow m, Alternative m) =>
--            k -> k -> D.Decode m (Row k VP) Double
-- sumCols k1 k2 = (+) <$> real k1 <*> real k2




-- test data

-- ro0 :: Row Int Value
-- ro0 = fromKVs [(0, VInt 32), (1, VChar 'z'), (2, VDouble pi)]



-- newtype Dec k v m a = Dec { unDec :: D.Decode m (Row k v) a } deriving (Functor, Applicative, Alternative)

-- runDec :: Dec k v m a -> Row k v -> m a
-- runDec = D.runDecode . unDec
