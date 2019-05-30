-----------------------------------------------------------------------------
-- |
-- Module      :  Heidi
-- Description :  tidy data in Haskell
-- Copyright   :  (c) Marco Zocca (2018-2019)
-- License     :  BSD-style
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- 
-----------------------------------------------------------------------------
module Heidi (
  -- * Frame
  Frame
  -- ** Construction
  , fromNEList, fromList
  -- ** Access
  , head, take, drop, zipWith, unionColsWith, numRows
  -- ** Filtering 
  , filter, filterByKey
  -- ** Grouping
  , groupWith
  -- ** Scans 
  , scanl, scanr
  -- ** Data tidying
  , spread, gather
  -- ** Relational operations
  , groupBy, innerJoin, leftOuterJoin
  -- ** Vector-related
  , toVector, fromVector  
  -- ** Generic encoding
  , gToFrame, gToFrameGT, HasGE, TC, VP
             ) where

import Core.Data.Frame (Frame, fromNEList, fromList, head, take, drop, zipWith, unionColsWith, numRows, filter, filterByKey, groupWith, scanl, scanr, spread, gather, groupBy, innerJoin, leftOuterJoin, toVector, fromVector)
import Core.Data.Frame.Generic (gToFrame, gToFrameGT)
import Data.Generics.Encode.Internal (HasGE, TC, VP)

import Prelude hiding (filter, zipWith, lookup, foldl, foldr, scanl, scanr, head, take, drop)
