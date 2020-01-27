{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
module Core.Data.Frame.PrettyPrint where

import GHC.Generics (Generic(..))
import qualified Data.Foldable as F (foldlM)
import Data.Function (on)
import Data.List (filter, sortBy)

-- boxes
import Text.PrettyPrint.Boxes (Box, Alignment, emptyBox, vcat, hcat, vsep, hsep, text, para, punctuateH, render, printBox, (<>), (<+>), (//), (/+/), top, left, right)

-- import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.GenericTrie as GT

import qualified Core.Data.Frame as CDF
import qualified Core.Data.Frame.Generic as CDF (gToFrameGT, gToRowGT)
import qualified Heidi.Data.Row.GenericTrie as GTR

import Prelude hiding ((<>))

{-

what's the best data structure for representing this kind of table display?

a trie with strings as keys and lists as values ?

+-------------+-----------------+
| Person      | House           |
+-------+-----+-------+---------+
| Name  | Age | Color | Price   |
+-------+-----+-------+---------+
| David | 63  | Green | $170000 |
| Ava   | 34  | Blue  | $115000 |
| Sonic | 12  | Green | $150000 |
+-------+-----+-------+---------+

(table example from colonnade : https://hackage.haskell.org/package/colonnade-1.2.0.2/docs/src/Colonnade.html#cap )

-}


{-

fold over a list of tries to update a PTree

[Trie [k] v] -> PTree v

foldl :: Foldable t => (b -> row -> b) -> b -> t row -> b

foldWithKey :: GT.TrieKey k => (k -> a -> r -> r) -> r -> Row k a -> r

-}

arr0, arr1 :: [Box]
arr0 = [text "moo", text "123123123"]
arr1 = [text "asdfasdfasdfasdf", text "z"]

-- justification is computed per-column by Box
box1 :: Alignment -> Box
box1 aln = hsep 2 top [c0, c1]
  where
    c0 = vsep 1 aln arr0
    c1 = vsep 1 aln arr1


data PTree k a =
     PColumn { plContents :: [Maybe a] }
   | PBranch { pbBranches :: M.Map k (PTree k a) }

-- layer k0 r0 = undefined
--   where
--     (lrow, rrow) = GTR.partitionWithKey (\k _ -> k0 == head k) r0
--     mlayer = M.singleton k0


-- peel r = GTR.fromList $ map g $ GTR.toList r
--   where
--     g (kss, v) | null ks = M.insert
--       -- let (k:ks) = kss
--       -- in if null ks
--       --    then ([k], Right v)
--       --    else (ks, Left v)

ptree0 :: PTree k a
ptree0 = PBranch M.empty

-- updateCol :: Show a => a -> PTree k1 a -> PTree k2 a
-- updateCol x (PColumn w0 xs) =
--   let
--     w = length $ show x
--     w1 = max w0 w
--   in PColumn w1 (x:xs) 

-- -- quiz: what's the "extrude" function that populates an M1 using an M2 ?

-- data M1 l a =
--     M1Leaf a
--   | M1Branch (M.Map l (M1 l a))

-- upsertM1 :: Ord k => (v -> a -> v) -> [k] -> a -> M1 k v -> M1 k v
-- upsertM1 f kss v m0 = go kss m0 where
--   go [] acc = case acc of
--     M1Leaf u    -> M1Leaf $ f u v
--     ma -> ma
--   go (k:ks) acc = case acc of
--     M1Branch m -> M1Branch $ M.insert k (go ks acc) m
--     l -> l


-- -- insertM1 kss v = go kss M.empty where
-- --   go [] _ = M1Leaf v
-- --   go (k:ks) acc = M1Branch $ M.insert k (go ks acc) acc

-- -- data M2 l a = M2 (M.Map [l] a)











-- newtype T v = T (GT.Trie [String] [v])

-- newtype Cols k v = Cols { unCols :: GT.Trie [k] v } deriving (Functor, Foldable, Traversable)

-- instance (GT.TrieKey k, Show k, Show v) => Show (Cols k v) where
--   show = show . GT.toList . unCols

-- empty :: GT.TrieKey k => Cols k v
-- empty = Cols GT.empty

-- insert :: GT.TrieKey k => [k] -> v -> Cols k v -> Cols k v
-- insert k v (Cols gt) = Cols $ GT.insert k v gt

-- fromList :: GT.TrieKey k => [([k], v)] -> Cols k v
-- fromList = Cols . GT.fromList




-- -- | number of characters of a Show instance
-- lshow :: Show a => a -> Int
-- lshow = length . show
