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
  Core.Data.Frame.List.head,
  Core.Data.Frame.List.take,
  Core.Data.Frame.List.drop, Core.Data.Frame.List.zipWith, numRows, 
  -- ** Filtering 
  Core.Data.Frame.List.filter, 
  -- *** 'D.Decode'-based filtering helpers
  filterA, 
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


-- | A 'Frame' is a list of rows.
newtype Frame row = Frame {
    tableRows :: [row] } deriving (Show, Functor, Foldable, Traversable)

head :: Frame row -> row
head = Prelude.head . tableRows

-- | Retain n rows
take :: Int -> Frame row -> Frame row
take n = Frame . Prelude.take n . tableRows

-- | Drop n rows
drop :: Int -> Frame row -> Frame row
drop n = Frame . Prelude.drop n . tableRows

zipWith :: (a -> b -> c) -> Frame a -> Frame b -> Frame c
zipWith f x y = fromList $ Prelude.zipWith f (tableRows x) (tableRows y)

fromList :: [row] -> Frame row
fromList = Frame

toList :: Frame row -> [row]
toList = tableRows

numRows :: Frame row -> Int
numRows = length . tableRows

filter :: (row -> Bool) -> Frame row -> Frame row
filter p = Frame . Prelude.filter p . tableRows


-- | This generalizes the list-based 'filter' function.
filterA :: Applicative f =>
           (row -> f Bool) -> Frame row -> f (Frame row)
filterA fm t = fromList <$> CM.filterM fm (toList t)






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
