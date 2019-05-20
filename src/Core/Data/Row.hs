{-# language TypeFamilies #-}
{-# language MultiParamTypeClasses #-}
-- {-# language ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
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
module Core.Data.Row
  (
  Row
  -- ,
  -- -- * Construction
  -- fromKVs,
  -- -- ** (unsafe)
  -- mkRow, 
  -- -- * Update
  -- insert, insertRowFun, insertRowFunM, 
  -- -- * Access
  -- toList, keys, elems,
  -- -- ** Decoders
  -- real, scientific, text, oneHot, 
  -- -- * Lookup
  -- lookup, lookupThrowM, lookupDefault, (!:), elemSatisfies, 
  -- -- * Set operations
  -- union, unionWith,
  -- -- * Traversals
  -- traverseWithKey,
  -- -- * Key constraint
  -- Key,
  )
  where

-- import Data.Typeable (Typeable)
-- import Control.Applicative (Alternative(..))

-- import Data.Hashable (Hashable(..))
-- import Control.Monad.Catch(Exception(..), MonadThrow(..))
-- import qualified Data.HashMap.Strict as HM
-- import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
-- import qualified Data.Text as T (pack)
-- import Data.Text (Text)

-- import Prelude hiding (lookup)

-- import qualified Data.Generics.Decode as D (Decode, runDecode, mkDecode)
-- import Data.Generics.Decode ((>>>))
-- import Data.Generics.Encode.Internal (VP, getIntM, getFloatM, getDoubleM, getScientificM, getStringM, getTextM, getOneHotM)
-- import Data.Generics.Encode.OneHot (OneHot)
-- import Core.Data.Row.Internal (KeyError(..))


class RowKey k v where
  type RowRep k :: * -> *


newtype Row k v = MkRow (RowRep k v)
