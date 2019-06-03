-----------------------------------------------------------------------------
-- |
-- Module      :  Heidi.Data.Frame.Algorithms.GenericTrie
-- Description :  GenericTrie-based dataframe algorithms
-- Copyright   :  (c) Marco Zocca (2018-2019)
-- License     :  BSD-style
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-----------------------------------------------------------------------------
module Heidi.Data.Frame.Algorithms.GenericTrie (
  -- ** Row-wise operations
  unionColsWith  
  -- ** Filtering 
  , filterByKey
  -- ** Data tidying
  , spread, gather
  -- ** Relational operations
  , groupBy, innerJoin, leftOuterJoin  
                                           ) where

import Data.Maybe (fromMaybe)
-- import Control.Applicative (Alternative(..))
import qualified Data.Foldable as F
-- import Data.Foldable (foldl, foldr, foldlM, foldrM)
-- import qualified Data.Vector as V
-- import qualified Data.Vector.Generic.Mutable as VGM
-- import qualified Data.Vector.Algorithms.Merge as V (sort, sortBy, Comparison)
-- import qualified Data.Text as T (pack)
-- import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable(..))
import qualified Data.Set as S (Set, fromList)
-- import Control.Monad.Primitive (PrimMonad(..), PrimState(..))
-- import Control.Monad.Catch(Exception(..), MonadThrow(..))
-- import Data.Scientific (Scientific, toRealFloat)
-- import Data.Typeable (Typeable)

import qualified Data.GenericTrie as GT

-- import qualified Data.Generics.Decode as D (Decode, runDecode)
-- import Data.Generics.Decode ((>>>))
import Core.Data.Frame (Frame, filter, fromList, zipWith)
import qualified Heidi.Data.Row.GenericTrie as GTR
-- import Core.Data.Row.Internal
-- import Data.Generics.Encode.Val (VP, getIntM, getFloatM, getDoubleM, getScientificM, getStringM, getTextM, getOneHotM)
-- import Data.Generics.Encode.OneHot (OneHot)

import Prelude hiding (filter, zipWith, lookup, foldl, foldr, scanl, scanr, head, take, drop)



-- | Merge two frames by taking the set union of the columns
unionColsWith :: (Eq k, GT.TrieKey k) =>
                 (v -> v -> v)   -- ^ Element combination function
              -> Frame (GTR.Row k v)
              -> Frame (GTR.Row k v)
              -> Frame (GTR.Row k v)
unionColsWith f = zipWith (GTR.unionWith f)


-- | Filter a 'Frame' according to predicate applied to an element pointed to by a given key.
--
-- >>> numRows <$> filterByKey "item" (/= "book") t0
-- Just 2
filterByKey :: (Eq k, GT.TrieKey k) =>
               k            -- ^ Key
            -> (v -> Bool)  -- ^ Predicate to be applied to the element
            -> Frame (GTR.Row k v)
            -> Maybe (Frame (GTR.Row k v))
filterByKey k ff = filter (k GTR.!: ff)



-- * Data tidying

-- | 'gather' moves column names into a "key" column, gathering the column values into a single "value" column
gather :: (Foldable t, Ord k, GT.TrieKey k) =>
          (k -> v)
       -> S.Set k     -- ^ set of keys to gather
       -> k           -- ^ "key" key           
       -> k           -- ^ "value" key
       -> t (GTR.Row k v) -- ^ input dataframe
       -> Frame (GTR.Row k v)
gather fk ks kKey kValue = fromList . F.foldMap f where
  f row = gather1 fk ks row kKey kValue

-- | gather one row into a list of rows
gather1 :: (Ord k, GT.TrieKey k) =>
           (k -> v)
        -> S.Set k     
        -> GTR.Row k v -- ^ row to look into
        -> k           -- ^ "key" key
        -> k           -- ^ "value" key
        -> [GTR.Row k v]
gather1 fk ks row kKey kValue = fromMaybe [] $ F.foldlM insf [] ks where
  rowBase = GTR.removeKnownKeys ks row
  lookupInsert k = do
    x <- GTR.lookup k row
    let 
      r'  = GTR.insert kKey   (fk k) rowBase
      r'' = GTR.insert kValue x r'
    pure r''
  insf acc k = do
    r' <- lookupInsert k
    pure $ r' : acc





-- | 'spread' moves the unique values of a key column into the column names, spreading the values of a value column across the new columns.
spread :: (GT.TrieKey k, Foldable t, Ord k, Ord v) =>
          (v -> k)
       -> k   -- ^ "key" key
       -> k   -- ^ "value" key
       -> t (GTR.Row k v)  -- ^ input dataframe
       -> Frame (GTR.Row k v)
spread fk k1 k2 = fromList . map funion . M.toList . F.foldl (spread1 fk k1 k2) M.empty
  where
    funion (km, vm) = GTR.union km vm
  
-- | spread1 creates a single row from multiple ones that share a subset of key-value pairs.
spread1 :: (Ord k, Ord v, GT.TrieKey k, Eq k) =>
           (v -> k)
        -> k
        -> k
        -> M.Map (GTR.Row k v) (GTR.Row k v)
        -> GTR.Row k v
        -> M.Map (GTR.Row k v) (GTR.Row k v)
spread1 fk k1 k2 hmacc row = M.insert rowBase kvNew hmacc where
  ks = S.fromList [k1, k2]
  rowBase = GTR.removeKnownKeys ks row
  hmv = GTR.maybeEmpty $ M.lookup rowBase hmacc
  kvNew = GTR.maybeEmpty $ do
    k <- GTR.lookup k1 row
    v <- GTR.lookup k2 row
    pure $ GTR.insert (fk k) v hmv




-- r0, r1, r2, r3 :: GTR.Row String String
-- r0 = GTR.fromKVs [
--     ("country", "A"), ("type", "cases"), ("count", "0.7")]
-- r1 = GTR.fromKVs [
--     ("country", "A"), ("type", "pop"), ("count", "19")]
-- r2 = GTR.fromKVs [
--     ("country", "B"), ("type", "cases"), ("count", "37")] 
-- r3 = GTR.fromKVs [
--     ("country", "B"), ("type", "pop"), ("count", "172")]    

-- -- frame0 :: [GTR.Row String String]
-- frame0 = fromList [r0, r1, r2, r3] 

-- fr1 = spread id "type" "count" frame0

-- fr2 = gather id (S.fromList ["cases", "pop"]) "type" "count" fr1





-- * Relational operations

-- | GROUP BY : given a key and a table that uses it, split the table in multiple tables, one per value taken by the key.
-- --
-- -- >>> numRows <$> (HM.lookup "129" $ groupBy "id.0" t0)
-- -- Just 2
groupBy :: (Foldable t, GT.TrieKey k, Hashable v, Eq k, Eq v) =>
           k  -- ^ Key to group by
        -> t (GTR.Row k v) -- ^ A @Frame (Row k v)@ can be used here
        -> HM.HashMap v (Frame (GTR.Row k v))
groupBy k tbl = fromList <$> groupL k tbl

groupL :: (Foldable t, GT.TrieKey k, Hashable v, Eq k, Eq v) =>
          k -> t (GTR.Row k v) -> HM.HashMap v [GTR.Row k v]
groupL k tbl = F.foldl insf HM.empty tbl where
  insf acc row = maybe acc (\v -> HM.insertWith (++) v [row] acc) (GTR.lookup k row)





joinWith :: (Foldable t, Hashable v, GT.TrieKey k, Eq v, Eq k) =>
            (GTR.Row k v -> [GTR.Row k v] -> [GTR.Row k v])
         -> k
         -> k
         -> t (GTR.Row k v)
         -> t (GTR.Row k v)
         -> Frame (GTR.Row k v)
joinWith f k1 k2 table1 table2 = fromList $ F.foldl insf [] table1 where
  insf acc row1 = maybe (f row1 acc) appendMatchRows (GTR.lookup k1 row1) where
    appendMatchRows v = map (GTR.union row1) mr2 ++ acc where
      mr2 = matchingRows k2 v table2   

-- | LEFT (OUTER) JOIN : given two dataframes and one key from each, compute the left outer join using the keys as relations.
leftOuterJoin :: (Foldable t, Hashable v, GT.TrieKey k, Eq v, Eq k) =>
                 k
              -> k
              -> t (GTR.Row k v)
              -> t (GTR.Row k v)
              -> Frame (GTR.Row k v)
leftOuterJoin = joinWith (:)


-- | INNER JOIN : given two dataframes and one key from each, compute the inner join using the keys as relations.
-- --
-- -- >>> head t0
-- -- [("id.0","129"),("qty","1"),("item","book")]
-- --
-- -- >>> head t1
-- -- [("id.1","129"),("price","100")]
-- -- 
-- -- >>> head $ innerJoin "id.0" "id.1" t0 t1
-- -- [("id.1","129"),("id.0","129"),("qty","5"),("item","book"),("price","100")]
innerJoin :: (Foldable t, Hashable v, GT.TrieKey k, Eq v, Eq k) =>
             k  -- ^ Key into the first table
          -> k  -- ^ Key into the second table
          -> t (GTR.Row k v)  -- ^ First dataframe
          -> t (GTR.Row k v)  -- ^ Second dataframe
          -> Frame (GTR.Row k v)
innerJoin = joinWith seq


   
matchingRows :: (Foldable t, Hashable v, GT.TrieKey k, Eq v, Eq k) =>
                k
             -> v
             -> t (GTR.Row k v)
             -> [GTR.Row k v]
matchingRows k v rows = fromMaybe [] (HM.lookup v rowMap) where
  rowMap = hjBuild k rows
{-# INLINE matchingRows #-}
    
-- | "build" phase of the hash-join algorithm
--
-- For a given key 'k' and a set of frame rows, populates a hashmap from the _values_ corresponding to 'k' to the corresponding rows.
hjBuild :: (Foldable t, Eq v, Eq k, Hashable v, GT.TrieKey k) =>
           k -> t (GTR.Row k v) -> HM.HashMap v [GTR.Row k v]
hjBuild k = F.foldl insf HM.empty where
  insf hmAcc row = maybe hmAcc (\v -> HM.insertWith (++) v [row] hmAcc) $ GTR.lookup k row
{-# INLINE hjBuild #-}

