{-# language DeriveFunctor, GeneralizedNewtypeDeriving, DeriveTraversable, DeriveDataTypeable #-}
-- {-# language ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Core.Data.Row.Internal
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
module Core.Data.Row.Internal where

import Data.Typeable (Typeable)
import Data.Hashable (Hashable(..))
import Control.Monad.Catch(Exception(..))


-- | Key exceptions 
data KeyError k = MissingKeyError k deriving (Show, Eq, Typeable)
instance (Show k, Typeable k) => Exception (KeyError k)
