{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
{-# language LambdaCase #-}
module Core.Data.Frame.PrettyPrint where

import GHC.Generics (Generic(..))
import qualified Data.Foldable as F (foldl', foldlM)
-- import Data.
import Data.Function (on)
import Data.List (filter, sortBy, groupBy)

-- boxes
import Text.PrettyPrint.Boxes (Box, Alignment, emptyBox, nullBox, vcat, hcat, vsep, hsep, text, para, punctuateH, render, printBox, (<>), (<+>), (//), (/+/), top, left, right)

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


--   +-------------+-----------------+
--   | Person      | House           |
--   +-------+-----+-------+---------+
--   | Name  | Age | Color | Price   |

box2 :: Box
box2 = l <+> r
  where
    l = text "Person" // (text "Name" <+> text "Age")
    r = text "House" // (text "Color" <+> text "Price")









data M k v = Ml v
           | Mb (M.Map k (M k v)) deriving (Functor, Foldable)
instance (Show k, Show v) => Show (M k v) where
  show = \case
    Ml x -> show x
    Mb m -> show $ M.toList m

empty :: M k v
empty = Mb M.empty

-- | Copy the contents of a list-indexed Row into a tree-shaped structure (for pretty-printing)
--
-- >>> unfold [("aa", 41), ("ab", 42)]
-- [('a',[('a',[('a',41)]),('b',42)])]  -- FIXME why 3 levels and not 2 ?!?
unfold :: (Foldable t, Ord k) =>
          t ([k], v) -- each GTR.Row is isomorphic to this parameter
       -> M k v
unfold kvs = foldl insf empty kvs
  where
    insf (Mb acc) (ks, v) = insert acc ks v
    insf _        _       = undefined -- FIXME

-- | Copy a single list-indexed value into a tree
--
-- >>> insert M.empty "abc" 42
-- [('a',[('b',[('c',42)])])]
insert :: Ord k => M.Map k (M k v) -> [k] -> v -> M k v
insert = go
  where
    go _ [] v = Ml v
    go m (k:ks) v = Mb $ M.insert k (go m ks v) m


-- data Tree a = Node {
--         rootLabel :: a,         -- ^ label value
--         subForest :: [Tree a]   -- ^ zero or more child trees

-- unfoldTree :: (b -> (a, [b])) -> b -> Tree a
-- unfoldTree f b = let (a, bs) = f b in Node a (unfoldForest f bs)
-- 
-- unfoldForest :: (b -> (a, [b])) -> [b] -> Forest a
-- unfoldForest f = map (unfoldTree f)


-- boxM (Ml s) = text s
-- boxM (Mb mm) = foldl ins nullBox mm
--   where
--     ins acc x = acc <+> boxM x



-- >>> groupSort ["aa", "ab", "cab", "xa", "cx"]
-- [["aa","ab"],["cab","cx"],["xa"]]
groupSort :: Ord a => [[a]] -> [[[a]]]
groupSort = groupSortBy head

groupSortBy :: Ord a1 => (a2 -> a1) -> [a2] -> [[a2]]
groupSortBy f = groupBy ((==) `on` f) . sortBy (compare `on` f)

-- render a column of a frame
columnBox :: (Foldable t, Show a, GT.TrieKey k) =>
             t (GTR.Row k a) -- ^ dataframe
          -> k -- ^ column key
          -> Box
columnBox rows k = foldl ins nullBox rows
  where
    ins acc row = acc // maybe (emptyBox 1 0) (text . show) (GTR.lookup k row)






-- | union of the set of keys across all rows
allKeys :: (GT.TrieKey k, Foldable f) => f (GTR.Row k v) -> [k]
allKeys = GTR.keys . GTR.keysOnly




-- data Sized a = Sized !Int a

-- annotateWithDepth :: (GT.TrieKey k) => GTR.Row [k] a -> GTR.Row [k] (Sized a)
-- annotateWithDepth = GTR.mapWithKey (\k v -> Sized (length k) v)
