{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
{-# language LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
module Core.Data.Frame.PrettyPrint where

import GHC.Generics (Generic(..))
import qualified Data.Foldable as F (foldl', foldlM)
-- import Data.
import Data.Function (on)
import Data.List (filter, sortBy, groupBy, intersperse)

-- boxes
import Text.PrettyPrint.Boxes (Box, Alignment, emptyBox, nullBox, vcat, hcat, vsep, hsep, text, para, punctuateH, render, printBox, (<>), (<+>), (//), (/+/), top, left, right, center1, center2, rows, cols)

-- import qualified Data.Text as T
-- containers
import qualified Data.Map as M
-- generic-trie
import qualified Data.GenericTrie as GT
-- unordered-containers
import qualified Data.HashMap.Strict as HM (HashMap, toList, keys, mapWithKey)

import qualified Core.Data.Frame as CDF
import qualified Core.Data.Frame.Generic as CDF (encode)
import Data.Generics.Encode.Internal (Heidi, toVal, Val(..), VP(..))
import qualified Data.Generics.Encode.OneHot as OH (OneHot)

import Prelude hiding ((<>))

{-
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


-- render the frame header
--
-- λ> printBox $ header $ toVal (E (Just 32) Nothing)
--       E
-- -------------
--   _0  |   _1
-- Maybe   Maybe
--  ----
--  Just
--   ()
-- λ>
header :: Val -> Box
header = \case
  VRec ty hm ->
    let
      bxs = values $ HM.mapWithKey (\k v -> text k /|/ header v) hm
    in
      -- dashesP bxs /|/ hSepList bxs
      text ty /|/ dashesP bxs /|/ hSepList bxs
  VPrim _ -> text "()"
  _ -> undefined -- TODO

values :: HM.HashMap k v -> [v]
values = map snd . HM.toList

dashesP :: [Box] -> Box
dashesP = punctuateH top (text "+") . map (\b -> dashes (cols b + 2))

dashes :: Int -> Box
dashes n = text $ replicate n '-'

hSepList :: [Box] -> Box
hSepList = hcat top . intersperse seph

seph :: Box
seph = text " | "

(/|/) :: Box -> Box -> Box
b1 /|/ b2 = vcat center1 [b1, b2]


data Z = Z Int Int
instance Show Z where
  show (Z a b) = unwords [show a, "\n", show b]

-- -- examples

data A0 = A0 deriving (Eq, Show, Generic, Heidi)
newtype A = A Int deriving (Eq, Show, Generic, Heidi)
newtype A2 = A2 { a2 :: Int } deriving (Eq, Show, Generic, Heidi)
data B = B Int Char deriving (Eq, Show, Generic, Heidi)
data B2 = B2 { b21 :: Int, b22 :: Char } deriving (Eq, Show, Generic, Heidi)
data C = C1 Int | C2 Char | C3 String deriving (Eq, Show, Generic, Heidi)
data D = D (Maybe Int) (Either Int String) deriving (Eq, Show, Generic, Heidi)
data E = E (Maybe Int) (Maybe Char) deriving (Eq, Show, Generic, Heidi)
data R = R { r1 :: B, r2 :: C } deriving (Eq, Show, Generic, Heidi)







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





-- -- >>> groupSort ["aa", "ab", "cab", "xa", "cx"]
-- -- [["aa","ab"],["cab","cx"],["xa"]]
-- groupSort :: Ord a => [[a]] -> [[[a]]]
-- groupSort = groupSortBy head

-- groupSortBy :: Ord a1 => (a2 -> a1) -> [a2] -> [[a2]]
-- groupSortBy f = groupBy ((==) `on` f) . sortBy (compare `on` f)

-- -- render a column of a frame
-- columnBox :: (Foldable t, Show a, GT.TrieKey k) =>
--              t (GTR.Row k a) -- ^ dataframe
--           -> k -- ^ column key
--           -> Box
-- columnBox rows k = foldl ins nullBox rows
--   where
--     ins acc row = acc // maybe (emptyBox 1 0) (text . show) (GTR.lookup k row)



-- -- | union of the set of keys across all rows
-- allKeys :: (GT.TrieKey k, Foldable f) => f (GTR.Row k v) -> [k]
-- allKeys = GTR.keys . GTR.keysOnly



-- data Sized a = Sized !Int a

-- annotateWithDepth :: (GT.TrieKey k) => GTR.Row [k] a -> GTR.Row [k] (Sized a)
-- annotateWithDepth = GTR.mapWithKey (\k v -> Sized (length k) v)
