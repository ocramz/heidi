{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Core.Data.Frame.List
-- Description :  List-based dataframe
-- Copyright   :  (c) Marco Zocca (2018-2019)
-- License     :  BSD-style
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- A general-purpose, row-oriented data frame, encoded as a list of rows
--
-----------------------------------------------------------------------------
module Core.Data.Frame.List (
  -- * Frame
  Frame,
  -- ** Construction
  fromList,
  -- ** Access
  Core.Data.Frame.List.head, take, drop, Core.Data.Frame.List.zipWith, numRows, 
  -- ** Filtering 
  filter, 
  -- *** 'D.Decode'-based filtering
  filterDecode, 
  -- **
  groupWith, 
  -- ** Scans (row-wise cumulative operations)
  Core.Data.Frame.List.scanl, Core.Data.Frame.List.scanr,
  -- ** Vector-related
  toVector, fromVector,
  -- *** Sorting
  ) where

import qualified Control.Monad as CM (filterM)
import Data.List (groupBy)

import qualified Data.Vector as V

import qualified Data.Generics.Decode as D (Decode, runDecode)

-- import Prelude hiding (zipWith)

-- | A 'Frame' is a non-empty list of rows.
newtype Frame row = Frame {
    tableRows :: [row] } deriving (Show, Functor, Foldable, Traversable)

head :: Frame row -> row
head = Prelude.head . tableRows

zipWith :: (a -> b -> c) -> Frame a -> Frame b -> Frame c
zipWith f x y = fromList $ Prelude.zipWith f (tableRows x) (tableRows y)

fromList :: [row] -> Frame row
fromList = Frame

toList :: Frame row -> [row]
toList = tableRows

numRows :: Frame row -> Int
numRows = length . tableRows


-- | This generalizes the list-based 'filter' function.
filterA :: Applicative f =>
           (row -> f Bool) -> Frame row -> f (Frame row)
filterA fm t = fromList <$> CM.filterM fm (toList t)



-- | Filter a 'Frame' by decoding row values.
--
-- This is an intermediate function that doesn't require fixing the row type within the 'Frame'.
--
-- NB: a 'D.Decode' returning 'Bool' can be declared via its Functor, Applicative and Alternative instances.
filterDecode :: Applicative f =>
                D.Decode f row Bool   -- ^ Row decoder
             -> Frame row
             -> f (Frame row)
filterDecode dec = filterA (D.runDecode dec)


-- | Left-associative scan
scanl :: (b -> a -> b) -> b -> Frame a -> Frame b
scanl f z tt = Frame $ Prelude.scanl f z (tableRows tt)

-- | Right-associative scan
scanr :: (a -> b -> b) -> b -> Frame a -> Frame b
scanr f z tt = Frame $ Prelude.scanr f z (tableRows tt)

-- | 'groupWith' takes row comparison function and a list and returns a list of lists such that the concatenation of the result is equal to the argument. Moreover, each sublist in the result contains only elements that satisfy the comparison. 
groupWith :: (row -> row -> Bool) -> Frame row -> [Frame row]
groupWith f t = Frame <$> groupBy f (tableRows t)




-- | Produce a 'Vector' of rows
toVector :: Frame row -> V.Vector row
toVector = V.fromList . tableRows

-- | Produce a Frame from a 'Vector' of rows
fromVector :: V.Vector row -> Frame row
fromVector = fromList . V.toList
