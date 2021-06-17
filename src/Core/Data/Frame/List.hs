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
  Frame(..),
  -- -- ** Construction
  -- frameFromList,
  -- ** Access
  Core.Data.Frame.List.head,
  Core.Data.Frame.List.take,
  Core.Data.Frame.List.drop,
  -- Core.Data.Frame.List.zipWith,
  numRows, 
  -- ** Filtering 
  Core.Data.Frame.List.filter, 
  -- -- *** 'D.Decode'-based filtering helpers
  -- filterA, 
  -- **
  groupWith, 
  -- ** Scans (row-wise cumulative operations)
  Core.Data.Frame.List.scanl, Core.Data.Frame.List.scanr,
  -- ** Vector-related
  toVector,
  -- fromVector,
  -- *** Sorting
  ) where

-- import qualified Control.Monad as CM (filterM)
import Data.Generics.Encode.Internal (Header(..))
import Data.List (groupBy)

import qualified Data.Vector as V


-- | A 'Frame' is a list of rows.
data Frame row = Frame {
  frameHeader :: Header String
    , tableRows :: [row] } deriving (Functor, Foldable, Traversable)

instance (Show r) => Show (Frame r) where
  show (Frame h rs) =  show rs -- FIXME add header machinery

head :: Frame row -> row
head = Prelude.head . tableRows

-- | Retain n rows
take :: Int -> Frame row -> Frame row
take n (Frame h rs) = Frame h $ Prelude.take n rs

-- | Drop n rows
drop :: Int -> Frame row -> Frame row
drop n (Frame h rs) = Frame h $ Prelude.drop n rs

-- zipWith :: (a -> b -> c) -> Frame a -> Frame b -> Frame c
-- zipWith f x y = frameFromList $ Prelude.zipWith f (tableRows x) (tableRows y)

-- frameFromList :: [row] -> Frame row
-- frameFromList = Frame

toList :: Frame row -> [row]
toList = tableRows

numRows :: Frame row -> Int
numRows = length . tableRows

filter :: (row -> Bool) -> Frame row -> Frame row
filter p (Frame h rs) = Frame h $ Prelude.filter p rs


-- -- | This generalizes the list-based 'filter' function.
-- filterA :: Applicative f =>
--            (row -> f Bool) -> Frame row -> f (Frame row)
-- filterA fm t = frameFromList <$> CM.filterM fm (toList t)






-- | Left-associative scan
scanl :: (b -> a -> b) -> b -> Frame a -> Frame b
scanl f z (Frame h rs) = Frame h $ Prelude.scanl f z rs

-- | Right-associative scan
scanr :: (a -> b -> b) -> b -> Frame a -> Frame b
scanr f z (Frame h rs) = Frame h $ Prelude.scanr f z rs

-- | 'groupWith' takes row comparison function and a list and returns a list of lists such that the concatenation of the result is equal to the argument. Moreover, each sublist in the result contains only elements that satisfy the comparison. 
groupWith :: (row -> row -> Bool) -> Frame row -> [Frame row]
groupWith f (Frame h rs) = Frame h <$> groupBy f rs




-- | Produce a 'Vector' of rows
toVector :: Frame row -> V.Vector row
toVector = V.fromList . tableRows

-- -- | Produce a Frame from a 'Vector' of rows
-- fromVector :: V.Vector row -> Frame row
-- fromVector = frameFromList . V.toList
