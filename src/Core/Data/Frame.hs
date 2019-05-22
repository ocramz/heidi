{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
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
  -- ** Folds
  foldl, foldr, foldlM, foldrM,
  -- ** Scans (row-wise cumulative operations)
  scanl, scanr,
  -- ** Relational operations
  groupBy, innerJoin, leftOuterJoin,   
  -- -- * Row
  -- Row,
  -- -- ** Construction
  -- fromKVs,
  -- -- *** (unsafe)
  -- mkRow, 
  -- -- ** Update
  -- insert, insertRowFun, insertRowFunM, 
  -- -- ** Access
  -- toList, keys, elems,
  -- -- *** Decoders
  -- D.Decode, D.mkDecode, D.runDecode, 
  -- real, scientific, text, oneHot, 
  -- -- ** Lookup
  -- HMR.lookup, lookupThrowM, lookupDefault, (!:), elemSatisfies, 
  -- -- ** Set operations
  -- union, unionWith,
  -- -- ** Traversals
  -- traverseWithKey,
  -- * One-Hot
  OneHot, 
  -- * Key constraint
  HMR.Key
  ) where

import Data.Maybe (fromMaybe)
-- import Control.Applicative (Alternative(..))
import qualified Data.Foldable as F
-- import qualified Data.Vector as V
-- import qualified Data.Text as T (pack)
-- import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import Data.Hashable (Hashable(..))
-- import Control.Monad.Catch(Exception(..), MonadThrow(..))
-- import Data.Scientific (Scientific, toRealFloat)
-- import Data.Typeable (Typeable)

-- import qualified Data.Generics.Decode as D (Decode, runDecode, mkDecode)
-- import Data.Generics.Decode ((>>>))
import qualified Core.Data.Row.HashMap as HMR
-- import Core.Data.Row.Internal
-- import Data.Generics.Encode.Val (VP, getIntM, getFloatM, getDoubleM, getScientificM, getStringM, getTextM, getOneHotM)
import Data.Generics.Encode.OneHot (OneHot)


import Prelude hiding (filter, zipWith, lookup, foldl, foldr, scanl, scanr, head, take, drop)

-- $setup
-- >>> let row0 = HMR.fromKVs [(0, 'a'), (3, 'b')] :: HMR.Row Int Char
-- >>> let row1 = HMR.fromKVs [(0, 'x'), (1, 'b'), (666, 'z')] :: HMR.Row Int Char
-- >>> let book1 = HMR.fromKVs [("item", "book"), ("id.0", "129"), ("qty", "1")]
-- >>> let book2 = HMR.fromKVs [("item", "book"), ("id.0", "129"), ("qty", "5")]
-- >>> let ball = HMR.fromKVs [("item", "ball"), ("id.0", "234"), ("qty", "1")]
-- >>> let bike = HMR.fromKVs [("item", "bike"), ("id.0", "410"), ("qty", "1")]
-- >>> let t0 = fromList [ book1, ball, bike, book2 ] :: Frame (HMR.Row String String)
-- >>> let r1 = HMR.fromKVs [("id.1", "129"), ("price", "100")]
-- >>> let r2 = HMR.fromKVs [("id.1", "234"), ("price", "50")]
-- >>> let r3 = HMR.fromKVs [("id.1", "3"), ("price", "150")]
-- >>> let r4 = HMR.fromKVs [("id.1", "99"), ("price", "30")]
-- >>> let t1 = fromList [ r1, r2, r3, r4 ] :: Frame (HMR.Row String String)



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
              -> Frame (HMR.Row k v)
              -> Frame (HMR.Row k v)
              -> Frame (HMR.Row k v)
unionColsWith f = zipWith (HMR.unionWith f)

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
            -> Frame (HMR.Row k v)
            -> Maybe (Frame (HMR.Row k v))
filterByKey k ff = filter (k HMR.!: ff)

-- | Left-associative fold
foldl :: (b -> a -> b) -> b -> Frame a -> b
foldl = F.foldl

-- | Right-associative fold
foldr :: (a -> b -> b) -> b -> Frame a -> b
foldr = F.foldr

-- | Left-associative monadic fold
foldlM :: (Monad m) => (b -> a -> m b) -> b -> Frame a -> m b
foldlM = F.foldlM

-- | Right-associative monadic fold
foldrM :: (Monad m) => (a -> b -> m b) -> b -> Frame a -> m b
foldrM = F.foldrM


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
numRows = length




-- gather = foldlM insf [] where
--   insf acc fr = do

lookupInsert :: (Eq k, Hashable k) =>
                k -> k -> k -> HMR.Row k k -> Maybe (HMR.Row k k)
lookupInsert k sKey sValue row = do
  x <- HMR.lookup k row
  let r'  = HMR.insert sKey k row
      r'' = HMR.insert sValue x r'
  pure r''
          


-- * Relational operations

-- | GROUP BY : given a key and a table that uses it, split the table in multiple tables, one per value taken by the key.
--
-- >>> numRows <$> (HM.lookup "129" $ groupBy "id.0" t0)
-- Just 2
groupBy :: (Foldable t, Hashable k, Hashable v, Eq k, Eq v) =>
           k  -- ^ Key to group by
        -> t (HMR.Row k v) -- ^ A @Frame (Row k v)@ can be used here
        -> HM.HashMap v (Frame (HMR.Row k v))
groupBy k tbl = fromList <$> groupL k tbl

groupL :: (Foldable t, Hashable k, Hashable v, Eq k, Eq v) =>
          k -> t (HMR.Row k v) -> HM.HashMap v [HMR.Row k v]
groupL k tbl = F.foldl insf HM.empty tbl where
  insf acc row = maybe acc (\v -> HM.insertWith (++) v [row] acc) (HMR.lookup k row)






-- | LEFT (OUTER) JOIN : given two dataframes and one key from each, compute the left outer join using the keys as relations.
leftOuterJoin :: (Foldable t, Hashable v, Hashable k, Eq v, Eq k) =>
                 k
              -> k
              -> t (HMR.Row k v)
              -> t (HMR.Row k v)
              -> Frame (HMR.Row k v)
leftOuterJoin k1 k2 table1 table2 = fromList $ F.foldl insf [] table1 where
  insf acc row1 = maybe (row1 : acc) appendMatchRows (HMR.lookup k1 row1) where
    appendMatchRows v = map (HMR.union row1) mr2 ++ acc where
      mr2 = matchingRows k2 v table2   




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
          -> t (HMR.Row k v)  -- ^ First dataframe
          -> t (HMR.Row k v)  -- ^ Second dataframe
          -> Frame (HMR.Row k v)
innerJoin k1 k2 table1 table2 = fromList $ F.foldl insf [] table1 where
  insf acc row1 = maybe acc appendMatchRows (HMR.lookup k1 row1) where
    appendMatchRows v = map (HMR.union row1) mr2 ++ acc where
      mr2 = matchingRows k2 v table2

   
matchingRows :: (Foldable t, Hashable v, Hashable k, Eq v, Eq k) =>
                k
             -> v
             -> t (HMR.Row k v)
             -> [HMR.Row k v]
matchingRows k v rows = fromMaybe [] (HM.lookup v rowMap) where
  rowMap = hjBuild k rows
{-# INLINE matchingRows #-}
    
-- | "build" phase of the hash-join algorithm
--
-- For a given key 'k' and a set of frame rows, populates a hashmap from the _values_ corresponding to 'k' to the corresponding rows.
hjBuild :: (Foldable t, Eq v, Eq k, Hashable v, Hashable k) =>
           k -> t (HMR.Row k v) -> HM.HashMap v [HMR.Row k v]
hjBuild k = F.foldl insf HM.empty where
  insf hmAcc row = maybe hmAcc (\v -> HM.insertWith (++) v [row] hmAcc) $ HMR.lookup k row
{-# INLINE hjBuild #-}










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


  


employee :: Frame (HMR.Row String String)
employee = fromList [e1, e2, e3, e4, e5, e6] where
  e1 = HMR.fromKVs [("name", "Rafferty"), ("id.dep", "31")]
  e2 = HMR.fromKVs [("name", "Jones"), ("id.dep", "33")]
  e3 = HMR.fromKVs [("name", "Heisenberg"), ("id.dep", "33")]
  e4 = HMR.fromKVs [("name", "Robinson"), ("id.dep", "34")]
  e5 = HMR.fromKVs [("name", "Smith"), ("id.dep", "34")]
  e6 = HMR.fromKVs [("name", "Williams")]   

department :: Frame (HMR.Row String String)
department = fromList [d1, d2, d3, d4] where
  d1 = HMR.fromKVs [("id.dep", "31"), ("dept", "Sales")]
  d2 = HMR.fromKVs [("id.dep", "33"), ("dept", "Engineering")]
  d3 = HMR.fromKVs [("id.dep", "34"), ("dept", "Clerical")]
  d4 = HMR.fromKVs [("id.dep", "35"), ("dept", "Marketing")]  
