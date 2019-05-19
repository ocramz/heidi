{-# language DeriveGeneric #-}
module Main where

import GHC.Generics (Generic)

-- import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM

import qualified Data.Text as T

import Core.Data.Frame
import Core.Data.Frame.Generic
import Data.Generics.Encode.Val (ToVal, TC(..), VP(..))

import Prelude hiding (filter, lookup)

main :: IO ()
main = pure ()


-- | Item
data Item a = Itm String a deriving (Eq, Show, Generic)
instance ToVal a => ToVal (Item a)

-- | Purchase
data Purchase a = Pur {
    date :: Int
  , pers :: String
  , item :: String
  , qty :: a
  } deriving (Eq, Show, Generic)
instance ToVal a => ToVal (Purchase a)

items :: [Item Double]
items = [i1, i2, i3] where
  i1 = Itm "computer" 1000
  i2 = Itm "car" 5000
  i3 = Itm "legal" 400

purchases :: [Purchase Double]
purchases = [p1, p2, p3, p4, p5] where
  p1 = Pur 7 "bob" "car" 1
  p2 = Pur 5 "alice" "car" 1
  p3 = Pur 4 "bob" "legal" 20
  p4 = Pur 3 "alice" "computer" 2
  p5 = Pur 1 "bob" "computer" 1

gItems, gPurchases :: Maybe (Frame (Row [TC] VP))
gItems = gToFrame items
gPurchases = gToFrame purchases

  
-- itemKey :: [TC]
-- itemKey = [TC "Purchase" "item"]

-- removeLegalExpenses :: Frame (Row [TC] VP) -> Maybe (Frame (Row [TC] VP))
-- removeLegalExpenses = filterByKey itemKey (/= VPString "legal")

-- joinTables :: (Foldable t, Hashable v, Eq v) =>
--               t (Row [TC] v) -> t (Row [TC] v) -> Frame (Row [TC] v)
-- joinTables = innerJoin k1 k2 where
--   k1 = [TC "Itm" "_0"]
--   k2 = itemKey


{-
Averaged across persons, excluding legal fees, how much money had each person spent by time 6?

item , price 
----------
computer , 1000 
car , 5000 
legal fees (1 hour) , 400

date , person , item-bought , units-bought 
------------------------------------
7 , bob , car , 1 
5 , alice , car , 1 
4 , bob , legal fees (1 hour) , 20 
3 , alice , computer , 2 
1 , bob , computer , 1 

It would be extra cool if you provided both an in-memory and a streaming solution.

Principles|operations it illustrates

Predicate-based indexing|filtering. Merging (called "joining" in SQL). Within- and across-group operations. Sorting. Accumulation (what Data.List calls "scanning"). Projection (both the "last row" and the "mean" operations). Statistics (the "mean" operation).

Solution and proposed algorithm (it's possible you don't want to read this)

The answer is $4000. That's because by time 6, Bob had bought 1 computer ($1000) and 20 hours of legal work (excluded), while Alice had bought a car ($5000) and two computers ($2000). In total they had spent $8000, so the across-persons average is $4000.

One way to compute that would be to:

    Delete any purchase of legal fees.
    Merge price and purchase data.
    Compute a new column, "money-spent" = units-bought price.
    Group by person.
    Within each group: Sort by date in increasing order.
    Compute a new column, "accumulated-spending" = running total of money spent.
    Keep the last row with a date no greater than 6; drop all others.
    Across groups, compute the mean of accumulated spending.

-}
