{-# language DeriveFunctor, GeneralizedNewtypeDeriving, DeriveTraversable, DeriveDataTypeable #-}
-- {-# language ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
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
-- Rows are internally represented with HashMaps; this format
-- supports the possibility of missing features in the dataset.
--
-----------------------------------------------------------------------------
module Core.Data.Row (
  Row,
  -- * Construction
  fromKVs,
  -- ** (unsafe)
  mkRow, 
  -- * Update
  insert, insertRowFun, insertRowFunM, 
  -- * Access
  toList, keys, elems,
  -- ** Decoders
  real, text, oneHot, 
  -- * Lookup
  lookup, lookupThrowM, lookupDefault, (!:), elemSatisfies, 
  -- * Set operations
  union, unionWith,
  -- * Traversals
  traverseWithKey,
  -- * Key constraint
  Key,
  ) where

import Data.Typeable (Typeable)
import Control.Applicative (Alternative(..))

import Data.Hashable (Hashable(..))
import Control.Monad.Catch(Exception(..), MonadThrow(..))
import qualified Data.HashMap.Strict as HM
import Data.Scientific (Scientific, toRealFloat)
import qualified Data.Text as T (pack)
import Data.Text (Text)

import Prelude hiding (lookup)

import qualified Data.Generics.Decode as D (Decode, runDecode, mkDecode)
import Data.Generics.Decode ((>>>))
import Data.Generics.Encode.Val (VP, getIntM, getFloatM, getDoubleM, getScientificM, getStringM, getTextM, getOneHotM)
import Data.Generics.Encode.OneHot (OneHot)

-- $setup
-- >>> let row0 = fromKVs [(0, 'a'), (3, 'b')] :: Row Int Char
-- >>> let row1 = fromKVs [(0, 'x'), (1, 'b'), (666, 'z')] :: Row Int Char

-- | A 'Row' type is internally a hashmap:
--
-- * /O(log n)/ random access
-- * /O(n log n)/ set operations
-- * Supports missing elements
newtype Row k v = Row { unRow :: HM.HashMap k v } deriving (Eq, Functor, Foldable, Traversable)
instance (Show k, Show v) => Show (Row k v) where
  show = show . HM.toList . unRow

-- | Construct a 'Row' from a list of key-element pairs.
--
-- >>> lookup 3 (fromKVs [(3,'a'),(4,'b')])
-- Just 'a'
-- >>> lookup 6 (fromKVs [(3,'a'),(4,'b')])
-- Nothing
fromKVs :: (Eq k, Hashable k) => [(k, v)] -> Row k v
fromKVs = Row . HM.fromList

-- | Wrap a HashMap into a Row constructor.
--
-- NB : This function is for internal use only. Do not use this function in application code, since it may break invariants such as key uniqueness.
mkRow :: HM.HashMap k v -> Row k v
mkRow = Row

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

-- lookupWith :: (Eq k, Hashable k) => (x -> k) -> x -> Row k v -> Maybe v
-- lookupWith f k = lookup (f k)

-- | Like 'lookup', but throws a 'MissingKeyError' if the lookup is unsuccessful
lookupThrowM :: (MonadThrow m, Key k) =>
                k -> Row k v -> m v
lookupThrowM k r = maybe (throwM $ MissingKeyError k) pure (lookup k r)

-- | A 'Key' must be 'Eq', 'Hashable', 'Show', 'Typeable'
class (Eq k, Hashable k, Show k, Typeable k) => Key k 

-- | Key exceptions 
data KeyError k = MissingKeyError k deriving (Show, Eq, Typeable)
instance (Show k, Typeable k) => Exception (KeyError k)



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





-- | Decode a value from a Row indexed at the given key (returns in a MonadThrow type)
decodeColM :: (MonadThrow m, Key k) =>
              k -> D.Decode m (Row k o) o
decodeColM k = D.mkDecode (lookupThrowM k)

-- | Decode a value from a Row indexed at the given key (returns in the Maybe monad)
decodeCol :: (Eq k, Hashable k) => k -> D.Decode Maybe (Row k o) o
decodeCol k = D.mkDecode (lookup k)



-- decInt :: D.Decode Maybe VP Int
-- decInt = D.mkDecode getInt
-- -- decInteger = D.mkDecode getInteger
-- decDouble :: D.Decode Maybe VP Double
-- decDouble = D.mkDecode getDouble
-- -- decChar = D.mkDecode getChar
-- -- decText = D.mkDecode getText

-- | Decode any real numerical value (integer, double or 'Scientific') into a Double
decodeRealM :: (Alternative m, MonadThrow m) => D.Decode m VP Double
decodeRealM =
  (fromIntegral <$> decIntM)     <|>
  decDoubleM                     <|>
  (toRealFloat <$> decScientificM) 

-- | Decode a string ('String' or 'Text') into a Text
decodeTextM :: (Alternative m, MonadThrow m) => D.Decode m VP Text
decodeTextM =
  (T.pack <$> decStringM) <|>
  decTextM


-- | Decode into 'MonadThrow'
decIntM :: MonadThrow m => D.Decode m VP Int
decIntM = D.mkDecode getIntM
decDoubleM :: MonadThrow m => D.Decode m VP Double
decDoubleM = D.mkDecode getDoubleM
decScientificM :: MonadThrow m => D.Decode m VP Scientific
decScientificM = D.mkDecode getScientificM
decFloatM :: MonadThrow m => D.Decode m VP Float
decFloatM = D.mkDecode getFloatM
decStringM :: MonadThrow m => D.Decode m VP String
decStringM = D.mkDecode getStringM
decTextM :: MonadThrow m => D.Decode m VP Text
decTextM = D.mkDecode getTextM
decOneHotM :: MonadThrow m => D.Decode m VP (OneHot Int)
decOneHotM = D.mkDecode getOneHotM

-- | Lookup and decode a real number
real :: (Key k, MonadThrow m, Alternative m) => k -> D.Decode m (Row k VP) Double
real k = decodeColM k >>> decodeRealM

-- | Lookup and decode a text string
text :: (Key k, MonadThrow m, Alternative m) => k -> D.Decode m (Row k VP) Text
text k = decodeColM k >>> decodeTextM

-- | Lookup and decode a one-hot encoded enum
oneHot :: (Key k, MonadThrow m) =>
          k -> D.Decode m (Row k VP) (OneHot Int)
oneHot k = decodeColM k >>> decOneHotM

-- -- example
-- sumCols :: (Key k, MonadThrow m, Alternative m) =>
--            k -> k -> D.Decode m (Row k VP) Double
-- sumCols k1 k2 = (+) <$> real k1 <*> real k2




-- test data

-- ro0 :: Row Int Value
-- ro0 = fromKVs [(0, VInt 32), (1, VChar 'z'), (2, VDouble pi)]



newtype Dec k v m a = Dec { unDec :: D.Decode m (Row k v) a } deriving (Functor, Applicative, Alternative)

runDec :: Dec k v m a -> Row k v -> m a
runDec = D.runDecode . unDec
