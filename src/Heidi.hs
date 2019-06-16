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
-- Heidi : tidy data in Haskell
-- 
-----------------------------------------------------------------------------
module Heidi (
  -- * Frame
  Frame
  -- ** Construction
  , fromList
  -- ** Access
  , head, take, drop, zipWith, numRows
  -- ** Filtering 
  , filter
  -- *** 'D.Decode'-based filtering
  , filterDecode
  -- ** Grouping
  , groupWith
  -- ** Scans 
  , scanl, scanr
  -- -- ** Data tidying
  -- , spread, gather
  -- -- ** Relational operations
  -- , groupBy, innerJoin, leftOuterJoin
  -- ** Vector-related
  , toVector, fromVector  
  -- ** Generic encoding
  , gToFrameHM, gToFrameGT, HasGE, TC, tcTyN, tcTyCon, mkTyN, mkTyCon, VP
  -- * Decode
  , D.Decode
             ) where

import Core.Data.Frame.List (Frame, fromList, head, take, drop, zipWith, numRows, filter, filterDecode, groupWith, scanl, scanr, toVector, fromVector)
-- import Core.Data.Frame (Frame, fromNEList, fromList, head, take, drop, zipWith, numRows, filter, filterDecode, groupWith, scanl, scanr, toVector, fromVector)
import Core.Data.Frame.Generic (gToFrameHM, gToFrameGT)
import Data.Generics.Encode.Internal (HasGE, TC, tcTyN, tcTyCon, mkTyN, mkTyCon, VP)
import qualified Data.Generics.Decode as D (Decode)


-- import Control.Monad.Catch (MonadThrow(..))
import Prelude hiding (filter, zipWith, lookup, foldl, foldr, scanl, scanr, head, take, drop)
