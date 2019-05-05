{-# language DeriveFunctor #-}
module Core.Data.PrefixTree (PFTree, Key, empty, lookup, insert, fromList) where

import Data.Fix (Fix(..), cata, cataM, ana, anaM, hylo, hyloM)

import GHC.Exts (groupWith)
import Prelude hiding (lookup)


newtype TrieF a x = TrieF [(a, x)] deriving (Eq, Show, Functor)
-- data Row a x = Row a x deriving (Eq, Show, Functor)

newtype Trie a = Trie (Fix (TrieF a)) deriving (Eq, Show)

cataTrie :: (TrieF a x -> x) -> Trie a -> x
cataTrie phi (Trie t) = cata phi t
anaTrie :: (x -> TrieF a x) -> x -> Trie a
anaTrie psi z = Trie $ ana psi z
hyloTrie :: (TrieF a c -> c) -> (x -> TrieF a x) -> x -> c
hyloTrie phi psi = cataTrie phi . anaTrie psi






fromListT :: Ord a => [[a]] -> Trie a
fromListT = anaTrie fromListFT

fromListFT :: Ord a => [[a]] -> TrieF a [[a]]
fromListFT ss =
  if null (head ss) 
  then TrieF [] -- leaf
  else
    let sss = groupWith head ss
    in TrieF $ mkBranch <$> sss

mkBranch :: [[a]] -> (a, [[a]])
mkBranch sss =
  let c = head (head sss) -- they're all the same
  in (c, tail <$> sss )





-- | Prefix tree
data PFTree a b = Node (Maybe b) (a -> Maybe (PFTree a b))  deriving (Functor)

-- | Empty prefix tree
empty :: PFTree a b
empty = Node Nothing (const Nothing)

-- | Lookup inside a 'PFTree' with a 'Key'
lookup :: Key a -> PFTree a b -> Maybe b
lookup [] (Node b _) = b
lookup (x:xs) (Node _ f) = case f x of
  Nothing -> Nothing
  Just pt -> lookup xs pt

-- | A 'Key' is just a synonym for a list
type Key a = [a]

-- | Insert an element at a given key in a 'PFTree'
insert :: Eq a => Key a -> b -> PFTree a b -> PFTree a b
insert [] b (Node _ f) = Node (Just b) f
insert (x:xs) b (Node mb f) =
  case f x of
    Nothing -> Node mb $ insf (insert xs b empty)
    Just pt -> Node mb $ insf (insert xs b pt)
    where
      insf z x' = if x' == x then Just z else f x'

-- | Populate a 'PFTree' from a Foldable (eg a list) of ('Key', value) pairs
fromList :: (Foldable t, Eq a) => t (Key a, b) -> PFTree a b
fromList = foldl insf empty
  where
    insf t (k, x) = insert k x t






-- * You could do the same as above using (co-)free (co-)algebras (see below) but I don't have time to debug `insert`
-- see blog : https://blog.jle.im/entry/tries-with-recursion-schemes.html

-- import Data.Fix (Fix(..), cata, cataM, ana, anaM, hylo, hyloM)


-- data PFTreeF a b x = Node (Maybe b) (a -> Maybe x) deriving (Functor)

-- newtype PFTree a b = PFTree (Fix (PFTreeF a b))



-- lookup :: [a] -> PFTree a b -> Maybe b
-- lookup ks t = cataPT lookupAlg t ks

-- lookupAlg :: PFTreeF a b ([a] -> Maybe b) -> [a] -> Maybe b
-- lookupAlg (Node v looks) kss = case kss of
--   []   -> v
--   k:ks -> case looks k of
--     Nothing -> Nothing
--     Just lf -> lf ks

-- emptyF :: PFTreeF a b x
-- emptyF = Node Nothing (const Nothing)


-- insert k v t = anaPT (\v -> insertCoalg t v k)

-- insertCoalg :: PFTreeF a b x -> b -> [a] -> PFTreeF a b x
-- insertCoalg (Node mb looks) b kss = case kss of
--   []   -> Node (Just b) looks
--   -- k:ks -> case looks k of
--   --   Nothing -> Node mb $ \k' ->
--   --     if k' == k then Just $ insertCoalg emptyF b ks else looks k'
--   --   Just pt -> Node mb $ \k' ->
--   --     if k' == k then Just $ insertCoalg pt b ks else looks k'

      
-- singleton :: b -> Key a -> PFTree a b
-- singleton v = anaPT (singletonCo v)

-- singletonCo :: b -> Key a -> PFTreeF a b (Key a)
-- singletonCo v = sco where
--   sco []     = Node (Just v) (const Nothing)
--   sco (_:ks) = Node Nothing (const $ Just ks)
