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
  head, take, drop, zipWith, numRows, 
  -- ** Filtering 
  filter, 
  -- *** 'D.Decode'-based filtering
  filterDecode, 
  -- **
  groupWith, 
  -- ** Scans (row-wise cumulative operations)
  scanl, scanr,

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
  -- -- * One-Hot
  -- OneHot, 
  -- -- * Key constraint
  -- HMR.Key
  -- ** Vector-related
  toVector, fromVector,
  -- *** Sorting
  ) where

import qualified Control.Monad as CM (filterM)
import Data.Maybe (fromMaybe)
-- import Control.Applicative (Alternative(..))
-- import qualified Data.Foldable as F
-- import Data.Foldable (foldl, foldr, foldlM, foldrM)
import qualified Data.Vector as V
-- import qualified Data.Vector.Generic.Mutable as VGM
-- import qualified Data.Vector.Algorithms.Merge as V (sort, sortBy, Comparison)
-- import qualified Data.Text as T (pack)
-- import Data.Text (Text)
-- import qualified Data.Map as M
-- import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE

-- import Control.Monad.Catch(Exception(..), MonadThrow(..))
-- import Data.Scientific (Scientific, toRealFloat)
-- import Data.Typeable (Typeable)

import qualified Data.Generics.Decode as D (Decode, runDecode)
-- import Data.Generics.Decode ((>>>))
-- import qualified Heidi.Data.Row.HashMap as HMR
-- import qualified Data.GenericTrie as GT
-- import Core.Data.Row.Internal
-- import Data.Generics.Encode.Internal (VP, getIntM, getFloatM, getDoubleM, getScientificM, getStringM, getTextM, getOneHotM)
-- import Data.Generics.Encode.OneHot (OneHot)


import Prelude hiding (filter, zipWith, lookup, foldl, foldr, scanl, scanr, head, take, drop)

-- $setup
-- >>> import qualified Heidi.Data.Row.HashMap as HMR
-- >>> let row0 = HMR.fromList [(0, 'a'), (3, 'b')] :: HMR.Row Int Char
-- >>> let row1 = HMR.fromList [(0, 'x'), (1, 'b'), (666, 'z')] :: HMR.Row Int Char
-- >>> let book1 = HMR.fromList [("item", "book"), ("id.0", "129"), ("qty", "1")]
-- >>> let book2 = HMR.fromList [("item", "book"), ("id.0", "129"), ("qty", "5")]
-- >>> let ball = HMR.fromList [("item", "ball"), ("id.0", "234"), ("qty", "1")]
-- >>> let bike = HMR.fromList [("item", "bike"), ("id.0", "410"), ("qty", "1")]
-- >>> let t0 = fromList [ book1, ball, bike, book2 ] :: Frame (HMR.Row String String)
-- >>> let r1 = HMR.fromList [("id.1", "129"), ("price", "100")]
-- >>> let r2 = HMR.fromList [("id.1", "234"), ("price", "50")]
-- >>> let r3 = HMR.fromList [("id.1", "3"), ("price", "150")]
-- >>> let r4 = HMR.fromList [("id.1", "99"), ("price", "30")]
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
    tableRows :: NE.NonEmpty row } deriving (Show, Functor, Foldable, Traversable)

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

toList :: Frame a -> [a]
toList = NE.toList . tableRows

-- | Zip two frames with a row combining function
zipWith :: (a -> b -> row)
        -> Frame a -> Frame b -> Frame row
zipWith f tt1 tt2 = Frame $ NE.zipWith f (tableRows tt1) (tableRows tt2)



-- | Filters a 'Frame' according to a predicate. Returns Nothing only if the resulting table is empty (i.e. if no rows satisfy the predicate).
--
filter :: (row -> Bool) -> Frame row -> Maybe (Frame row)
filter ff = fromNEList . NE.filter ff . tableRows

-- | This generalizes the list-based 'filter' function.
filterA :: Applicative f =>
           (row -> f Bool) -> Frame row -> f (Maybe (Frame row))
filterA fm t = fromNEList <$> CM.filterM fm (toList t)



-- | Filter a 'Frame' by decoding row values.
--
-- This is an intermediate function that doesn't require fixing the row type within the 'Frame'.
--
-- NB: a 'D.Decode' returning 'Bool' can be declared via its Functor, Applicative and Alternative instances.
filterDecode :: Applicative f =>
                D.Decode f row Bool   -- ^ Row decoder
             -> Frame row
             -> f (Maybe (Frame row))
filterDecode dec = filterA (D.runDecode dec)


-- filterInt2 k1 k2 =
--   filterDecode ((>=) <$> HMR.scientific k1 <*> HMR.scientific k2)



-- | Left-associative scan
scanl :: (b -> a -> b) -> b -> Frame a -> Frame b
scanl f z tt = Frame $ NE.scanl f z (tableRows tt)

-- | Right-associative scan
scanr :: (a -> b -> b) -> b -> Frame a -> Frame b
scanr f z tt = Frame $ NE.scanr f z (tableRows tt)

-- | 'groupWith' takes row comparison function and a list and returns a list of lists such that the concatenation of the result is equal to the argument. Moreover, each sublist in the result contains only elements that satisfy the comparison. 
groupWith :: (row -> row -> Bool) -> Frame row -> [Frame row]
groupWith f t = Frame <$> NE.groupBy f (tableRows t)

-- | 'groupWithM' uses a comparison function that Maybe returns a Bool. This is useful when used in conjuction with lookup-based logic.
groupWithM :: (row -> row -> Maybe Bool) -> Frame row -> [Frame row]
groupWithM fm = groupWith f' where
  f' r1 r2 = fromMaybe False (fm r1 r2)

-- | /O(n)/ Count the number of rows in the table
--
-- >>> numRows t0
-- 4
numRows :: Frame row -> Int 
numRows = length



-- | Produce a 'Vector' of rows
toVector :: Frame row -> V.Vector row
toVector = V.fromList . NE.toList . tableRows

-- | Produce a Frame from a 'Vector' of rows
fromVector :: V.Vector row -> Maybe (Frame row)
fromVector = fromNEList . V.toList





