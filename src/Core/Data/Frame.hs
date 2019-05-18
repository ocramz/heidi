{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving #-}
{-# language DeriveDataTypeable, ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- {-# OPTIONS_HADDOCK show-extensions #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Core.Data.Frame
-- Description :  A sparse dataframe
-- Copyright   :  (c) Marco Zocca (2018-2019)
-- License     :  BSD-style
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- A general-purpose, row-oriented data frame.
--
-- As it is common in the sciences, the dataframe should be taken to contain
-- experimental datapoints as its rows, each being defined by a number of /features/.
--
-- Since the rows are internally represented with HashMaps, this format
-- supports the possibility of missing features in the dataset.
--
-----------------------------------------------------------------------------
module Core.Data.Frame (
  -- * Frame
  Frame,
  -- ** Construction
  fromNEList, fromList,
  -- ** Access
  head, take, drop, zipWith, unionColsWith, numRows, 
  -- ** Filtering 
  filter, filterByKey,
  -- ** Scans (row-wise cumulative operations)
  scanl, scanr, 
  -- * Row
  Row,
  -- ** Construction 
  fromKVs,
  -- *** (unsafe)
  mkRow, 
  -- ** Access
  keys, elems, toList, 
  -- ** Traversal
  traverseWithKey, 
  -- ** Lookup 
  lookup, -- lookupDefault,
  -- ** Insertion 
  insert, insertRowFun, insertRowFunM, 
  -- ** Set-like row operations
  union, unionWith,
  -- ** Row functions
  elemSatisfies, (!:),  
  -- * Relational operations
  groupBy, innerJoin, 
  -- * Decode
  D.Decode, real, text
  ) where

import Data.Maybe (fromMaybe)
import Control.Applicative (Alternative(..))
import qualified Data.Foldable as F
-- import qualified Data.Vector as V
import qualified Data.Text as T (pack)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import Data.Hashable (Hashable(..))
import Control.Monad.Catch(Exception(..), MonadThrow(..))
import Data.Scientific (Scientific, toRealFloat)
import Data.Typeable (Typeable)
import qualified Data.Generics.Decode as D (Decode, runDecode, mkDecode)
import Data.Generics.Decode ((>>>))
-- import Analyze.Common (Key, MissingKeyError(..))
import Data.Generics.Encode.Val (VP, getIntM, getFloatM, getDoubleM, getScientificM, getStringM, getTextM, getOneHotM)
import Data.Generics.Encode.OneHot (OneHot)


import Prelude hiding (filter, zipWith, lookup, scanl, scanr, head, take, drop)

-- $setup
-- >>> let row0 = fromKVs [(0, 'a'), (3, 'b')] :: Row Int Char
-- >>> let row1 = fromKVs [(0, 'x'), (1, 'b'), (666, 'z')] :: Row Int Char
-- >>> let book1 = fromKVs [("item", "book"), ("id.0", "129"), ("qty", "1")]
-- >>> let book2 = fromKVs [("item", "book"), ("id.0", "129"), ("qty", "5")]
-- >>> let ball = fromKVs [("item", "ball"), ("id.0", "234"), ("qty", "1")]
-- >>> let bike = fromKVs [("item", "bike"), ("id.0", "410"), ("qty", "1")]
-- >>> let t0 = fromList [ book1, ball, bike, book2 ] :: Frame (Row String String)
-- >>> let r1 = fromKVs [("id.1", "129"), ("price", "100")]
-- >>> let r2 = fromKVs [("id.1", "234"), ("price", "50")]
-- >>> let r3 = fromKVs [("id.1", "3"), ("price", "150")]
-- >>> let r4 = fromKVs [("id.1", "99"), ("price", "30")]
-- >>> let t1 = fromList [ r1, r2, r3, r4 ] :: Frame (Row String String)





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
type Key k = (Eq k, Hashable k, Show k, Typeable k)

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

decodeColM :: (MonadThrow m, Key k) =>
              k -> D.Decode m (Row k o) o
decodeColM k = D.mkDecode (lookupThrowM k)

decodeCol :: (Eq k, Hashable k) => k -> D.Decode Maybe (Row k o) o
decodeCol k = D.mkDecode (lookup k)



-- decInt :: D.Decode Maybe VP Int
-- decInt = D.mkDecode getInt
-- -- decInteger = D.mkDecode getInteger
-- decDouble :: D.Decode Maybe VP Double
-- decDouble = D.mkDecode getDouble
-- -- decChar = D.mkDecode getChar
-- -- decText = D.mkDecode getText

-- | Decode any real numerical value (integer or double) into a Double
decodeRealM :: (Alternative m, MonadThrow m) => D.Decode m VP Double
decodeRealM =
  (fromIntegral <$> decIntM)     <|>
  decDoubleM                     <|>
  (toRealFloat <$> decScientificM) 

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

-- | Creates or updates a column with a function of the whole row
insertRowFun :: (Eq k, Hashable k) => (Row k v -> v) -> k -> Row k v -> Row k v
insertRowFun f knew row = insert knew (f row) row

-- | Monadic version of 'insertRowFun'
insertRowFunM :: (Eq k, Hashable k, Monad m) => (Row k v -> m v) -> k -> Row k v -> m (Row k v)
insertRowFunM fm knew row = do
  y <- fm row
  pure $ insert knew y row




-- [NOTE : table Alternative instance] 
-- 
-- https://github.com/Gabriel439/Haskell-Bears-Library/blob/master/src/Bears.hs
--
-- 'Table' has Applicative and Alternative instances
-- -- *  for Alternative, we need the possibility of an empty table (to implement `empty`). Currently this is impossible due to the 'NonEmpty' list implementation.

-- [NOTE : column universe and table pretty printing]
--
-- Currently this 'Table' implementation doesn't know anything of its row type, including the type of its keys and values.
-- To pretty-print our tables, we'd like instead to know the "universe of columns", i.e. all possible columns used in every row (or at least in the first N rows)



-- | A 'Frame' is a non-empty list of rows.
newtype Frame row = Frame {
    -- nFrameRows :: Maybe Int  -- ^ Nothing means unknown
    tableRows :: NE.NonEmpty row } deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Take the first row of a 'Frame'
--
-- >>> head (fromList [row0, row1]) == row0
-- True
head :: Frame row -> row
head = NE.head . tableRows

-- | Take the first @n@ rows of a Frame
take :: Int -> Frame r -> [r]
take n = NE.take n . tableRows

-- | Drop the first @n@ rows of a Frame
drop :: Int -> Frame r -> [r]
drop n = NE.drop n . tableRows

-- | Construct a table given a non-empty list of rows
--
-- >>> (head <$> fromNEList [row0, row1]) == Just row0
-- True
-- >>> fromNEList []
-- Nothing
fromNEList :: [row] -> Maybe (Frame row)
fromNEList l = Frame <$> NE.nonEmpty l

-- | Construct a table given a list of rows. Crashes if the input list is empty
fromList :: [row] -> Frame row
fromList = Frame . NE.fromList 

-- | Zip two frames with a row combining function
zipWith :: (a -> b -> row)
        -> Frame a -> Frame b -> Frame row
zipWith f tt1 tt2 = Frame $ NE.zipWith f (tableRows tt1) (tableRows tt2)

-- | Merge two frames by taking the set union of the columns
unionColsWith :: (Eq k, Hashable k) =>
                 (v -> v -> v)   -- ^ Element combination function
              -> Frame (Row k v)
              -> Frame (Row k v)
              -> Frame (Row k v)
unionColsWith f = zipWith (unionWith f)

-- | Filters a 'Frame' according to a predicate. Returns Nothing only if the resulting table is empty (i.e. if no rows satisfy the predicate).
--
filter :: (row -> Bool) -> Frame row -> Maybe (Frame row)
filter ff = fromNEList . NE.filter ff . tableRows

-- | Filter a 'Frame' according to predicate applied to an element pointed to by a given key.
--
-- >>> numRows <$> filterByKey "item" (/= "book") t0
-- Just 2
filterByKey :: (Eq k, Hashable k) =>
               k            -- ^ Key
            -> (v -> Bool)  -- ^ Predicate to be applied to the element
            -> Frame (Row k v)
            -> Maybe (Frame (Row k v))
filterByKey k ff = filter (k !: ff)


-- | Left-associative scan
scanl :: (b -> a -> b) -> b -> Frame a -> Frame b
scanl f z tt = Frame $ NE.scanl f z (tableRows tt)

-- | Right-associative scan
scanr :: (a -> b -> b) -> b -> Frame a -> Frame b
scanr f z tt = Frame $ NE.scanr f z (tableRows tt)

-- | /O(n)/ Count the number of rows in the table
--
-- >>> numRows t0
-- 4
numRows :: Frame row -> Int 
numRows = length . tableRows



-- * Relational operations

-- | GROUP BY : given a key and a table that uses it, split the table in multiple tables, one per value taken by the key.
--
-- >>> numRows <$> (HM.lookup "129" $ groupBy "id.0" t0)
-- Just 2
groupBy :: (Foldable t, Hashable k, Hashable v, Eq k, Eq v) =>
           k  -- ^ Key to group by
        -> t (Row k v) -- ^ A @Frame (Row k v)@ can be used here
        -> HM.HashMap v (Frame (Row k v))
groupBy k tbl = fromList <$> groupL k tbl

groupL :: (Foldable t, Hashable k, Hashable v, Eq k, Eq v) =>
          k -> t (Row k v) -> HM.HashMap v [Row k v]
groupL k tbl = F.foldl insf HM.empty tbl where
  insf acc row = maybe acc (\v -> HM.insertWith (++) v [row] acc) (lookup k row)




-- join f k1 k2 table1 table2 = fromList $ F.foldl insf [] table1 where
--   insf acc row1 = f acc (lookup k1 row1) where
--     appendMatchRows v = map (union row1) mr2 ++ acc where
--       mr2 = matchingRows k2 v table2




-- | INNER JOIN : given two dataframes and one key from each, compute the inner join using the keys as relations.
--
-- >>> head t0
-- [("id.0","129"),("qty","1"),("item","book")]
--
-- >>> head t1
-- [("id.1","129"),("price","100")]
-- 
-- >>> head $ innerJoin "id.0" "id.1" t0 t1
-- [("id.1","129"),("id.0","129"),("qty","5"),("item","book"),("price","100")]
innerJoin :: (Foldable t, Hashable v, Hashable k, Eq v, Eq k) =>
             k  -- ^ Key into the first table
          -> k  -- ^ Key into the second table
          -> t (Row k v)  -- ^ First dataframe
          -> t (Row k v)  -- ^ Second dataframe
          -> Frame (Row k v)
innerJoin k1 k2 table1 table2 = fromList $ F.foldl insf [] table1 where
  insf acc row1 = maybe acc appendMatchRows (lookup k1 row1) where
    appendMatchRows v = map (union row1) mr2 ++ acc where
      mr2 = matchingRows k2 v table2
      
matchingRows :: (Foldable t, Hashable v, Hashable k, Eq v, Eq k) =>
                k
             -> v
             -> t (Row k v)
             -> [Row k v]
matchingRows k v rows = fromMaybe [] (HM.lookup v rowMap) where
  rowMap = hjBuild k rows
    
-- | "build" phase of the hash-join algorithm
--
-- For a given key 'k' and a set of frame rows, populates a hashmap from the _values_ corresponding to 'k' to the corresponding rows.
hjBuild :: (Foldable t, Eq v, Eq k, Hashable v, Hashable k) =>
           k -> t (Row k v) -> HM.HashMap v [Row k v]
hjBuild k = F.foldl insf HM.empty where
  insf hmAcc row = maybe hmAcc (\v -> HM.insertWith (++) v [row] hmAcc) $ lookup k row
    










-- -- test data


-- e0 :: Frame (Row String String)
-- e0 = fromList [r] where
--   r = fromKVs [("name", "Smith"), ("id.dep", "34")]
 
-- e0' :: Frame (Row String String)
-- e0' = fromList [r] where
--   r = fromKVs [("name", "Smith")] 

-- d0 :: Frame (Row String String)
-- d0 = fromList [r] where
--   r = fromKVs [("id.dep", "34"), ("dept", "Clerical")]


  


-- employee :: Frame (Row String String)
-- employee = fromList [e1, e2, e3, e4, e5, e6] where
--   e1 = fromKVs [("name", "Rafferty"), ("id.dep", "31")]
--   e2 = fromKVs [("name", "Jones"), ("id.dep", "33")]
--   e3 = fromKVs [("name", "Heisenberg"), ("id.dep", "33")]
--   e4 = fromKVs [("name", "Robinson"), ("id.dep", "34")]
--   e5 = fromKVs [("name", "Smith"), ("id.dep", "34")]
--   e6 = fromKVs [("name", "Williams")]   

-- department :: Frame (Row String String)
-- department = fromList [d1, d2, d3, d4] where
--   d1 = fromKVs [("id.dep", "31"), ("dept", "Sales")]
--   d2 = fromKVs [("id.dep", "33"), ("dept", "Engineering")]
--   d3 = fromKVs [("id.dep", "34"), ("dept", "Clerical")]
--   d4 = fromKVs [("id.dep", "35"), ("dept", "Marketing")]  
