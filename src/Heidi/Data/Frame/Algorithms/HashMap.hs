-----------------------------------------------------------------------------
-- |
-- Module      :  Heidi.Data.Frame.Algorithms.HashMap
-- Description :  HashMap-based dataframe algorithms
-- Copyright   :  (c) Marco Zocca (2018-2019)
-- License     :  BSD-style
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-----------------------------------------------------------------------------
module Heidi.Data.Frame.Algorithms.HashMap (
  -- ** Row-wise operations
  unionColsWith
  -- ** Filtering 
  , filterByKey
  -- ** Data tidying
  , spreadWith, gatherWith
  -- ** Relational operations
  , groupBy, innerJoin, leftOuterJoin  
                                           ) where

import Data.Maybe (fromMaybe)
-- import Control.Applicative (Alternative(..))
import qualified Data.Foldable as F (foldMap, foldl', foldlM)
-- import Data.Foldable (foldl, foldr, foldlM, foldrM)
-- import qualified Data.Vector.Generic.Mutable as VGM
-- import qualified Data.Vector.Algorithms.Merge as V (sort, sortBy, Comparison)
-- import qualified Data.Text as T (pack)
-- import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable(..))
import Data.Hashable.Time (Hashable(..))
import qualified Data.Set as S (Set, fromList)
-- import Control.Monad.Primitive (PrimMonad(..), PrimState(..))
-- import Control.Monad.Catch(Exception(..), MonadThrow(..))
-- import Data.Scientific (Scientific, toRealFloat)
-- import Data.Typeable (Typeable)

import qualified Data.Generics.Decode as D (Decode, runDecode)
-- import Data.Generics.Decode ((>>>))
import Core.Data.Frame.List (Frame, filter, fromList, zipWith)
import qualified Heidi.Data.Row.HashMap as HMR
-- import qualified Data.GenericTrie as GT
-- import Core.Data.Row.Internal
-- import Data.Generics.Encode.Val (VP, getIntM, getFloatM, getDoubleM, getScientificM, getStringM, getTextM, getOneHotM)
-- import Data.Generics.Encode.OneHot (OneHot)

import Prelude hiding (filter, zipWith, lookup, foldl, foldr, scanl, scanr, head, take, drop)


-- | Merge two frames by taking the set union of the columns
unionColsWith :: (Eq k, Hashable k) =>
                 (v -> v -> v)   -- ^ Element combination function
              -> Frame (HMR.Row k v)
              -> Frame (HMR.Row k v)
              -> Frame (HMR.Row k v)
unionColsWith f = zipWith (HMR.unionWith f)




-- | Filter a 'Frame' according to predicate applied to an element pointed to by a given key.
--
-- >>> numRows <$> filterByKey "item" (/= "book") t0
-- Just 2
filterByKey :: (Eq k, Hashable k) =>
               k            -- ^ Key
            -> (v -> Bool)  -- ^ Predicate to be applied to the element
            -> Frame (HMR.Row k v)
            -> Frame (HMR.Row k v)
filterByKey k ff = filter (k HMR.!: ff)



-- * Data tidying

-- | 'gatherWith' moves column names into a "key" column, gathering the column values into a single "value" column
gatherWith :: (Foldable t, Hashable k, Ord k) =>
              (k -> v)
           -> S.Set k     -- ^ set of keys to gather
           -> k           -- ^ "key" key
           -> k           -- ^ "value" key
           -> t (HMR.Row k v) -- ^ input dataframe
           -> Frame (HMR.Row k v)
gatherWith fk ks kKey kValue = fromList . F.foldMap f where
  f row = gather1 fk ks row kKey kValue

-- | gather one row into a list of rows
gather1 :: (Ord k, Hashable k) =>
           (k -> v)
        -> S.Set k
        -> HMR.Row k v -- ^ row to look into
        -> k           -- ^ "key" key
        -> k           -- ^ "value" key
        -> [HMR.Row k v]
gather1 fk ks row kKey kValue = fromMaybe [] $ F.foldlM insf [] ks where
  rowBase = HMR.removeKnownKeys ks row
  lookupInsert k = do
    x <- HMR.lookup k row
    let
      r'  = HMR.insert kKey   (fk k) rowBase
      r'' = HMR.insert kValue x r'
    pure r''
  insf acc k = do
    r' <- lookupInsert k
    pure $ r' : acc
{-# inline gather1 #-}





-- | 'spreadWith' moves the unique values of a key column into the column names, spreading the values of a value column across the new columns.
spreadWith :: (Foldable t, Hashable k, Ord k, Ord v) =>
              (v -> k)
           -> k   -- ^ "key" key
           -> k   -- ^ "value" key
           -> t (HMR.Row k v)  -- ^ input dataframe
           -> Frame (HMR.Row k v)
spreadWith fk k1 k2 = fromList . map funion . M.toList . F.foldl' (spread1 fk k1 k2) M.empty
  where
    funion (km, vm) = HMR.union km vm

-- | spread1 creates a single row from multiple ones that share a subset of key-value pairs.
spread1 :: (Ord k, Ord v, Hashable k, Eq k) =>
           (v -> k)
        -> k
        -> k
        -> M.Map (HMR.Row k v) (HMR.Row k v)
        -> HMR.Row k v
        -> M.Map (HMR.Row k v) (HMR.Row k v)
spread1 fk k1 k2 hmacc row = M.insert rowBase kvNew hmacc where
  ks = S.fromList [k1, k2]
  rowBase = HMR.removeKnownKeys ks row
  hmv = HMR.maybeEmpty $ M.lookup rowBase hmacc
  kvNew = HMR.maybeEmpty $ do
    k <- HMR.lookup k1 row
    v <- HMR.lookup k2 row
    pure $ HMR.insert (fk k) v hmv
{-# inline spread1 #-}        




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
groupL k tbl = F.foldl' insf HM.empty tbl where
  insf acc row = maybe acc (\v -> HM.insertWith (++) v [row] acc) (HMR.lookup k row)
{-# inline groupL #-}  





joinWith' f k1 k2 table1 table2 = fromList $ F.foldl' insf [] table1 where
  insf acc row1 = maybe (f row1 acc) appendMatchRows (HMR.lookup k1 row1) where
    appendMatchRows v = map (HMR.union row1) mr2 ++ acc where
      mr2 = matchingRows k2 v table2




joinWith :: (Foldable t, Hashable v, Hashable k, Eq v, Eq k) =>
            (HMR.Row k v -> [HMR.Row k v] -> [HMR.Row k v])
         -> k
         -> k
         -> t (HMR.Row k v)
         -> t (HMR.Row k v)
         -> Frame (HMR.Row k v)
joinWith f k1 k2 table1 table2 = fromList $ F.foldl' insf [] table1 where
  insf acc row1 = maybe (f row1 acc) appendMatchRows (HMR.lookup k1 row1) where
    appendMatchRows v = map (HMR.union row1) mr2 ++ acc where
      mr2 = matchingRows k2 v table2

-- | LEFT (OUTER) JOIN : given two dataframes and one key from each, compute the left outer join using the keys as relations.
leftOuterJoin :: (Foldable t, Hashable v, Hashable k, Eq v, Eq k) =>
                 k
              -> k
              -> t (HMR.Row k v)
              -> t (HMR.Row k v)
              -> Frame (HMR.Row k v)
leftOuterJoin = joinWith (:)


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
innerJoin = joinWith (\_ x -> x)



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
hjBuild k = F.foldl' insf HM.empty where
  insf hmAcc row = maybe hmAcc (\v -> HM.insertWith (++) v [row] hmAcc) $ HMR.lookup k row
{-# INLINE hjBuild #-}
