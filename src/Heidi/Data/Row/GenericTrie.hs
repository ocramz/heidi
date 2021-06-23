


{-# language DeriveTraversable #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# options_ghc -Wno-unused-imports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Heidi.Data.Row.GenericTrie
-- Description :  A sparse dataframe row, based on GenericTrie
-- Copyright   :  (c) Marco Zocca (2018-2019)
-- License     :  BSD-style
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Rows are internally represented with prefix trees ("tries"), as provided by the
-- @generic-trie@ library; in addition to supporting the possibility of missing features in the dataset, tries provide fast insertion and lookup functionality when keyed with structured datatypes (such as lists or trees).
--
-----------------------------------------------------------------------------
module Heidi.Data.Row.GenericTrie (
    Row
    -- * Construction
  , rowFromList, empty
  -- ** (unsafe)
  , mkRow
  -- * Update
  , insert, insertMany, insertWith
  -- * Access
  , toList, keys
  -- * Filtering
  , delete, filterWithKey, filterWithKeyPrefix, filterWithKeyAny
  , deleteMany
  -- * Partitioning
  , partitionWithKey, partitionWithKeyPrefix
  -- -- ** Decoders
  -- , real, scientific, text, string, oneHot
  -- * Lookup
  , lookup
  -- , lookupThrowM
  , (!:), elemSatisfies
  -- ** Lookup utilities
  , maybeEmpty
  -- ** Comparison by lookup
  , eqByLookup, eqByLookups
  , compareByLookup
  -- * Set operations
  , union, unionWith
  , intersection, intersectionWith
  -- * Maps
  , mapWithKey
  -- * Folds
  , foldWithKey, keysOnly
  -- * Traversals
  , traverseWithKey
  -- * Lens combinators
  -- ** Traversals
  , int, bool, float, double, char, string, text, scientific, oneHot, day, utcTime, timeOfDay, localTime, timeZone, nominalDiffTime, diffTime, universalTime
  -- ** Getters
  , real, txt
  -- , flag
  -- ** Combinators
  , at, keep
  -- *** Combinators for list-indexed rows
  , atPrefix, eachPrefixed, foldPrefixed
  ) where

import qualified Control.Applicative as A (empty)
import Control.Applicative ((<|>))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (Any(..), All(..), First(..))
-- import Data.Semigroup (Endo)
-- import Data.Typeable (Typeable)
-- import Control.Applicative (Alternative(..))
import qualified Data.Foldable as F
-- import Control.Monad (filterM)
-- generic-trie
import qualified Data.GenericTrie as GT
-- exceptions
-- import Control.Monad.Catch (MonadThrow(..))
-- microlens
import Lens.Micro (Lens', Traversal', Getting, (^.), (^?), (<&>), _Just, Getting, traversed, folded, to, has, (.~), failing)
-- -- microlens-th
-- import Lens.Micro.TH (makeLenses)
-- scientific
import Data.Scientific (Scientific)
-- text
import Data.Text (Text)
-- time
import Data.Time (Day, UTCTime,  TimeOfDay, LocalTime, TimeZone, NominalDiffTime, DiffTime, UniversalTime)
import qualified Data.Text as T (pack, unpack)


-- import qualified Data.Generics.Decode as D (Decode, mkDecode)
-- import Data.Generics.Decode ((>>>))
import Data.Generics.Encode.Internal (VP, vpInt, vpFloat, vpDouble, vpString, vpChar, vpText, vpBool, vpScientific, vpOneHot, vpDay, vpUTCTime, vpTimeOfDay, vpLocalTime, vpTimeZone, vpNominalDiffTime, vpDiffTime, vpUniversalTime)
import Data.Generics.Encode.OneHot (OneHot)
-- import Data.Generics.Codec
-- import Core.Data.Row.Internal (KeyError(..))

import Prelude hiding (any, lookup)


-- $setup
-- >>> import Data.Generics.Encode.Internal (VP)
-- >>> let row0 = fromList [(0, 'a'), (3, 'b')] :: Row Int Char
-- >>> let row1 = fromList [(0, 'x'), (1, 'b'), (666, 'z')] :: Row Int Char


-- | A 'Row' type is internally a Trie:
--
-- * Fast random access
-- * Fast set operations
-- * Supports missing elements
newtype Row k v = Row { _unRow :: GT.Trie k v } deriving (Functor, Foldable, Traversable)
instance (GT.TrieKey k) => Semigroup (Row k v) where
  Row r1 <> Row r2 = Row $ r1 `GT.union` r2
instance GT.TrieKey k => Monoid (Row k v) where mempty = Row GT.empty
-- makeLenses ''Row
instance (GT.TrieKey k, Show k, Show v) => Show (Row k v) where
  show = show . GT.toList . _unRow

instance (GT.TrieKey k, Eq k, Eq v) => Eq (Row k v) where
  r1 == r2 = F.toList r1 == F.toList r2

instance (GT.TrieKey k, Eq k, Eq v, Ord k, Ord v) => Ord (Row k v) where
  r1 <= r2 = F.toList r1 <= F.toList r2

-- | Focus on a given column
--
-- NB : setting a 'Nothing' value removes the entry
at :: GT.TrieKey k =>
      k -- ^ column index
   -> Lens' (Row k a) (Maybe a)
at k f m = f mv <&> \case
    Nothing -> maybe m (const (delete k m)) mv
    Just v' -> insert k v' m
    where mv = lookup k m
{-# INLINABLE at #-}

-- | 'atPrefix' : a Lens' that takes a key prefix and relates a row having lists as keys and the subset of columns corresponding to keys having that prefix
atPrefix :: (GT.TrieKey k, Eq k) =>
            [k] -- ^ key prefix of the columns of interest
         -> Lens' (Row [k] v) [v]
atPrefix k f m = f vs <&> \case
  [] -> if null kvs then m else deleteMany ks m
  vs' -> insertMany (zip ks vs') m
  where
    kvs = toList $ filterWithKeyPrefix k m
    (ks, vs) = unzip kvs

{- | Focus on all elements that share a common key prefix

e.g.

@
>>> :t \k -> 'Lens.Micro.toListOf' (eachPrefixed k . vpBool)
(GT.TrieKey k, Eq k) => [k] -> Row [k] VP -> [Bool]
@
-}
eachPrefixed :: (GT.TrieKey k, Eq k) =>
                [k] -- ^ key prefix of the columns of interest
             -> Traversal' (Row [k] v) v
eachPrefixed k = atPrefix k . traversed

-- | Extract all elements that share a common key prefix into a monoidal value (e.g. a list)
foldPrefixed :: (GT.TrieKey k, Eq k, Monoid r) =>
                [k] -- ^ key prefix of the columns of interest
             -> Getting r (Row [k] v) v
foldPrefixed k = atPrefix k . folded

-- foldingPrefixed f k = atPrefix k . folding f

-- any :: Eq a => a -> a -> Any
-- any v = Any . (== v)

-- | Helper for filtering 'Frame's
--
-- e.g.
--
-- >>> :t \k -> keep (text k) (== "hello")
--   :: GT.TrieKey k => k -> Row k VP -> Bool
keep :: Getting Any row a
     -> (a -> b) -- ^ e.g. a predicate
     -> row
     -> Bool
keep l f = has (l . to f)


-- keep :: (Eq a) => Getting Any row a -> a -> row -> Bool
-- keep l v = has (l . to (== v))


-- ** Getters

-- | Lookup a real number at the given index.
--
-- Matches `Double`, `Float`, `Int` and `Scientific` values.
real :: GT.TrieKey k => k -> Row k VP -> Maybe Double
real k r = g1 <|> g2 <|> g3 <|> g4
  where
    g1 = r ^? int k . to fromIntegral
    g2 = r ^? double k
    g3 = r ^? float k . to (fromRational . toRational)
    g4 = r ^? scientific k . to (fromRational . toRational)

-- | Look up a text string at the given index.
--
-- Matches `String` and `Text` values.
txt :: GT.TrieKey k => k -> Row k VP -> Maybe Text
txt k r = g1 <|> g2
  where
    g1 = r ^? string k . to T.pack
    g2 = r ^? text k

-- -- | Lookup a "flag" at the given index.
-- --
-- -- `Bool`ean values and 
-- flag :: GT.TrieKey k => k -> Row k VP -> Maybe Bool
-- flag k r = g1 <|> g2
--   where
--     g1 = r ^? bool k
--     g2 = r ^? char k . to f
--     f c = elem c ['y', 'Y']

-- class Contains a where
--   contains_ :: GT.TrieKey k => k -> Traversal' (Row k VP) a

-- instance Contains Int where contains_ = int
-- instance Contains Char where contains_ = char

-- contains :: (Contains a, Eq a, GT.TrieKey k, Monoid r, Foldable t) =>
--             k
--          -> t a
--          -> Getting r (Row k VP) Bool
-- contains k xs = contains_ k . to (`elem` xs)

-- ** Lenses

-- | Decode a 'Bool' from the given column index
bool :: GT.TrieKey k => k -> Traversal' (Row k VP) Bool
bool k = at k . _Just . vpBool
-- | Decode a 'Int' from the given column index
int :: GT.TrieKey k => k -> Traversal' (Row k VP) Int
int k = at k . _Just . vpInt
-- | Decode a 'Float' from the given column index
float :: GT.TrieKey k => k -> Traversal' (Row k VP) Float
float k = at k . _Just . vpFloat
-- | Decode a 'Double' from the given column index
double :: GT.TrieKey k => k -> Traversal' (Row k VP) Double
double k = at k . _Just . vpDouble
-- | Decode a 'Char' from the given column index
char :: GT.TrieKey k => k -> Traversal' (Row k VP) Char
char k = at k . _Just . vpChar
-- | Decode a 'String' from the given column index
string :: GT.TrieKey k => k -> Traversal' (Row k VP) String
string k = at k . _Just . vpString
-- | Decode a 'Text' from the given column index
text :: GT.TrieKey k => k -> Traversal' (Row k VP) Text
text k = at k . _Just . vpText
-- | Decode a 'Scientific' from the given column index
scientific :: GT.TrieKey k => k -> Traversal' (Row k VP) Scientific
scientific k = at k . _Just . vpScientific
-- | Decode a 'OneHot' from the given column index
oneHot :: GT.TrieKey k => k -> Traversal' (Row k VP) (OneHot Int)
oneHot k = at k . _Just . vpOneHot
-- | Decode a 'Day' from the given column index
day :: GT.TrieKey k => k -> Traversal' (Row k VP) Day
day k = at k . _Just . vpDay
-- | Decode a 'UTCTime' from the given column index
utcTime :: GT.TrieKey k => k -> Traversal' (Row k VP) UTCTime
utcTime k = at k . _Just . vpUTCTime
-- | Decode a 'TimeOfDay' from the given column index
timeOfDay :: GT.TrieKey k => k -> Traversal' (Row k VP) TimeOfDay
timeOfDay k = at k . _Just . vpTimeOfDay
-- | Decode a 'LocalTime' from the given column index
localTime :: GT.TrieKey k => k -> Traversal' (Row k VP) LocalTime
localTime k = at k . _Just . vpLocalTime
-- | Decode a 'TimeZone' from the given column index
timeZone :: GT.TrieKey k => k -> Traversal' (Row k VP) TimeZone
timeZone k = at k . _Just . vpTimeZone
-- | Decode a 'NominalDiffTime' from the given column index
nominalDiffTime :: GT.TrieKey k => k -> Traversal' (Row k VP) NominalDiffTime
nominalDiffTime k = at k . _Just . vpNominalDiffTime
-- | Decode a 'DiffTime' from the given column index
diffTime :: GT.TrieKey k => k -> Traversal' (Row k VP) DiffTime
diffTime k = at k . _Just . vpDiffTime
-- | Decode a 'UniversalTime' from the given column index
universalTime :: GT.TrieKey k => k -> Traversal' (Row k VP) UniversalTime
universalTime k = at k . _Just . vpUniversalTime






-- | Construct a 'Row' from a list of key-element pairs.
--
-- >>> lookup 3 (rowFromList [(3,'a'),(4,'b')])
-- Just 'a'
-- >>> lookup 6 (rowFromList [(3,'a'),(4,'b')])
-- Nothing
rowFromList :: GT.TrieKey k => [(k, v)] -> Row k v
rowFromList = Row . GT.fromList

-- | Construct a 'Row' from a trie (unsafe).
mkRow :: GT.Trie k v -> Row k v
mkRow = Row

-- | An empty row
empty :: GT.TrieKey k => Row k v
empty = Row GT.empty

-- | Access the key-value pairs contained in the 'Row'
toList :: GT.TrieKey k => Row k v -> [(k ,v)]
toList = GT.toList . _unRow

-- | Lookup the value stored at a given key in a row
--
-- >>> lookup 0 row0
-- Just 'a'
-- >>> lookup 1 row0
-- Nothing
lookup :: (GT.TrieKey k) => k -> Row k v -> Maybe v
lookup k = GT.lookup k . _unRow

liftLookup :: GT.TrieKey k =>
              (a -> b -> c) -> k -> Row k a -> Row k b -> Maybe c
liftLookup f k r1 r2 = f <$> lookup k r1 <*> lookup k r2


-- | Compares for ordering two rows by the values indexed at a specific key.
--
-- Returns Nothing if the key is not present in either row.
compareByLookup :: (GT.TrieKey k, Eq k, Ord a) =>
                   k -> Row k a -> Row k a -> Maybe Ordering
compareByLookup = liftLookup compare

-- | Compares two rows by the values indexed at a specific key.
--
-- Returns Nothing if the key is not present in either row.
eqByLookup :: (GT.TrieKey k, Eq k, Eq a) =>
              k -> Row k a -> Row k a -> Maybe Bool
eqByLookup = liftLookup (==)

-- | Compares two rows by the values indexed at a set of keys.
--
-- Returns Nothing if a key in either row is not present.
eqByLookups :: (Foldable t, GT.TrieKey k, Eq k, Eq a) =>
               t k -> Row k a -> Row k a -> Maybe Bool
eqByLookups ks r1 r2 = F.foldlM insf True ks where
  insf b k = pure (((&&)) b) <*> eqByLookup k r1 r2

-- -- | Like 'lookup', but throws a 'KeyError' if the lookup is unsuccessful
-- lookupThrowM :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k) =>
--                 k -> Row k v -> m v
-- lookupThrowM k r = maybe (throwM $ MissingKeyError k) pure (lookup k r)

-- | Returns an empty row if the argument is Nothing.
maybeEmpty :: GT.TrieKey k => Maybe (Row k v) -> Row k v
maybeEmpty = fromMaybe empty

-- | List the keys of a given row
--
-- >>> keys row0
-- [0,3]
keys :: GT.TrieKey k => Row k v -> [k]
keys = map fst . toList

-- | Takes the union of a Foldable container of 'Row's and discards the values
keysOnly :: (GT.TrieKey k, Foldable f) => f (Row k v) -> Row k ()
keysOnly ks = () <$ F.foldl' union empty ks

-- | Returns a new 'Row' that doesn't have a given key-value pair
delete :: GT.TrieKey k =>
          k       -- ^ Key to remove
       -> Row k v
       -> Row k v
delete k (Row gt) = Row $ GT.delete k gt

-- | Produce a new 'Row' such that its keys do _not_ belong to a certain set.
deleteMany :: (GT.TrieKey k, Foldable t) => t k -> Row k v -> Row k v
deleteMany ks r = foldl (flip delete) r ks

-- | Map over all elements with a function of both the key and the value
mapWithKey :: GT.TrieKey k => (k -> a -> b) -> Row k a -> Row k b
mapWithKey ff (Row gt) =
  runIdentity $ Row <$> GT.traverseWithKey (\k v -> pure (ff k v)) gt


-- | Filter a row by applying a predicate to its keys and corresponding elements.
--
-- NB : filtering _retains_ the elements that satisfy the predicate.
filterWithKey :: GT.TrieKey k => (k -> v -> Bool) -> Row k v -> Row k v
filterWithKey ff (Row gt) = Row $ GT.filterWithKey ff gt

-- | Retains the entries for which the given list is a prefix of the indexing key
filterWithKeyPrefix :: (GT.TrieKey a, Eq a) =>
                       [a] -- ^ key prefix
                    -> Row [a] v
                    -> Row [a] v
filterWithKeyPrefix kpre = filterWithKey (\k _ -> kpre `isPrefixOf` k)

-- | Partition a 'Row' into two new ones, such as the elements that satisfy the predicate will end up in the _left_ row.
partitionWithKey :: GT.TrieKey k =>
                    (k -> v -> Bool) -- ^ predicate
                 -> Row k v
                 -> (Row k v, Row k v)
partitionWithKey qf = foldWithKey insf (empty, empty)
  where
    insf k v (lacc, racc) | qf k v    = (insert k v lacc, racc)
                          | otherwise = (lacc, insert k v racc)

-- | Uses 'partitionWithKey' internally
partitionWithKeyPrefix :: (GT.TrieKey a, Eq a) =>
                          [a] -- ^ key prefix
                       -> Row [a] v
                       -> (Row [a] v, Row [a] v)
partitionWithKeyPrefix kpre = partitionWithKey (\k _ -> kpre `isPrefixOf` k)

-- | Retains the entries for which the given item appears at any position in the indexing key
filterWithKeyAny :: (GT.TrieKey a, Eq a) => a -> Row [a] v -> Row [a] v
filterWithKeyAny kany = filterWithKey (\k _ -> kany `elem` k)




-- alter k m = fromMaybe m $ do
--   v <- lookup k m 
--   delete k m

-- alter k f t =
--   case f (lookup k t) of
--     Just v' -> insert k v' t
--     Nothing -> delete k t

-- modify k v = 


-- | Insert a key-value pair into a row and return the updated one
-- 
-- >>> keys $ insert 2 'y' row0
-- [0,2,3]
insert :: (GT.TrieKey k) => k -> v -> Row k v -> Row k v
insert k v = Row . GT.insert k v . _unRow

insertMany :: (GT.TrieKey k, Foldable t) => t (k, v) -> Row k v -> Row k v
insertMany kvs r = foldl (\acc (k, v) -> insert k v acc) r kvs

-- | Insert a key-value pair into a row and return the updated one, or updates the value by using the combination function.
insertWith :: (GT.TrieKey k) => (v -> v -> v) -> k -> v -> Row k v -> Row k v
insertWith f k v = Row . GT.insertWith f k v . _unRow

-- | Fold over a row with a function of both key and value
foldWithKey :: GT.TrieKey k => (k -> a -> r -> r) -> r -> Row k a -> r
foldWithKey fk z (Row gt) = GT.foldWithKey fk z gt

-- | Traverse a 'Row' using a function of both the key and the element.
traverseWithKey :: (Applicative f, GT.TrieKey k) => (k -> a -> f b) -> Row k a -> f (Row k b)
traverseWithKey f r = Row <$> GT.traverseWithKey f (_unRow r)


-- | Set union of two rows
--
-- >>> keys $ union row0 row1
-- [0,1,3,666]
union :: (GT.TrieKey k) => Row k v -> Row k v -> Row k v
union r1 r2 = Row $ GT.union (_unRow r1) (_unRow r2)

-- | Set union of two rows, using a combining function for equal keys
unionWith :: (GT.TrieKey k) =>
             (v -> v -> v) -> Row k v -> Row k v -> Row k v
unionWith f r1 r2 = Row $ GT.unionWith f (_unRow r1) (_unRow r2)

-- | Set intersection of two rows
intersection :: GT.TrieKey k => Row k v -> Row k b -> Row k v
intersection r1 r2 = Row $ GT.intersection (_unRow r1) (_unRow r2)

-- | Set intersections of two rows, using a combining function for equal keys
intersectionWith :: GT.TrieKey k => (a -> b -> v) -> Row k a -> Row k b -> Row k v
intersectionWith f r1 r2 = Row $ GT.intersectionWith f (_unRow r1) (_unRow r2)


-- | Looks up a key from a row and applies a predicate to its value (if this is found). If no value is found at that key the function returns False.
--
-- This function is meant to be used as first argument to 'filter'.
--
-- >>> elemSatisfies (== 'a') 0 row0
-- True
-- >>> elemSatisfies (== 'a') 42 row0
-- False
elemSatisfies :: (GT.TrieKey k) => (a -> Bool) -> k -> Row k a -> Bool
elemSatisfies f k row = maybe False f (lookup k row)

-- | Inline synonym for 'elemSatisfies'
(!:) :: (GT.TrieKey k) => k -> (a -> Bool) -> Row k a -> Bool
k !: f = elemSatisfies f k

-- -- | Lookup a value from a Row indexed at the given key (returns in a MonadThrow type)
-- lookupColM :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k) =>
--               k -> D.Decode m (Row k o) o
-- lookupColM k = D.mkDecode (lookupThrowM k)

-- -- -- | Lookup a value from a Row indexed at the given key (returns in the Maybe monad)
-- -- lookupCol :: GT.TrieKey k => k -> D.Decode Maybe (Row k o) o
-- -- lookupCol k = D.mkDecode (lookup k)





-- -- * Decoders

-- -- | Lookup and decode a real number
-- real :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k, Alternative m) =>
--         k -> D.Decode m (Row k VP) Double
-- real k = lookupColM k >>> realM

-- -- | Lookup and decode a real 'Scientific' value
-- scientific :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k, Alternative m) =>
--               k -> D.Decode m (Row k VP) Scientific
-- scientific k = lookupColM k >>> scientificM

-- -- | Lookup and decode a text string (defaults to Text)
-- text :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k, Alternative m) =>
--         k -> D.Decode m (Row k VP) Text
-- text k = lookupColM k >>> textM

-- -- | Lookup and decode a text string (defaults to 'String')
-- string :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k, Alternative m) =>
--           k -> D.Decode m (Row k VP) String
-- string k = lookupColM k >>> stringM

-- -- | Lookup and decode a one-hot encoded enum
-- oneHot :: (MonadThrow m, Show k, Typeable k, GT.TrieKey k) =>
--           k -> D.Decode m (Row k VP) (OneHot Int)
-- oneHot k = lookupColM k >>> oneHotM




-- -- spork k1 k2 = (>) <$> real k1 <*> real k2


