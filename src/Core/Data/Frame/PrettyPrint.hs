{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
module Core.Data.Frame.PrettyPrint where

import qualified Data.Foldable as F (foldlM)
-- import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.GenericTrie as GT

import qualified Core.Data.Frame as CDF
import qualified Heidi.Data.Row.GenericTrie as GTR


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
| Sonia | 12  | Green | $150000 |
+-------+-----+-------+---------+
-}


-- quiz: what's the "extrude" function that populates an M1 using an M2 ?

data M1 l a =
    M1Leaf a
  | M1Branch (M.Map l (M1 l a))

upsertM1 :: Ord l => (a -> p -> a) -> [l] -> p -> M1 l a -> M1 l a
upsertM1 f kss v m0 = go kss m0 where
  go [] acc = case acc of
    M1Leaf u    -> M1Leaf $ f u v
    ma -> ma
  go (k:ks) acc = case acc of
    M1Branch m -> M1Branch $ M.insert k (go ks acc) m
    l -> l
    

-- insertM1 kss v = go kss M.empty where
--   go [] _ = M1Leaf v 
--   go (k:ks) acc = M1Branch $ M.insert k (go ks acc) acc  

data M2 l a = M2 (M.Map [l] a)  











newtype T v = T (GT.Trie [String] [v])




newtype Cols k v = Cols { unCols :: GT.Trie [k] v } deriving (Functor, Foldable, Traversable)

instance (GT.TrieKey k, Show k, Show v) => Show (Cols k v) where
  show = show . GT.toList . unCols

empty :: GT.TrieKey k => Cols k v
empty = Cols GT.empty

insert :: GT.TrieKey k => [k] -> v -> Cols k v -> Cols k v
insert k v (Cols gt) = Cols $ GT.insert k v gt

fromList :: GT.TrieKey k => [([k], v)] -> Cols k v
fromList = Cols . GT.fromList




-- | number of characters of a Show instance
lshow :: Show a => a -> Int
lshow = length . show
