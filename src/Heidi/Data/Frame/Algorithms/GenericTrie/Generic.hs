{-# language LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
module Heidi.Data.Frame.Algorithms.GenericTrie.Generic where

-- containers
import qualified Data.Set as S
-- generic-trie
import qualified Data.GenericTrie as GT
-- text
import qualified Data.Text as T (Text, pack, unpack)

import Heidi.Data.Row.GenericTrie (Row)
import Heidi.Data.Frame.Algorithms.GenericTrie (spreadWith, gatherWith)
import Core.Data.Frame.List (Frame)
-- import Data.Generics.Codec ()

import Data.Generics.Encode.Internal (VP(..), TC(..), tcTyCon, tcTyN, mkTyCon, mkTyN)



-- -- a user shouldn\t have to manipulate TCs

-- spread :: Foldable t => [TC] -> [TC] -> t (Row [TC] VP) -> Frame (Row [TC] VP)
-- spread = spreadWith valueToKey

-- gather :: Foldable t =>
--           S.Set [TC]
--        -> [TC]
--        -> [TC]
--        -> t (Row [TC] VP)
--        -> Frame (Row [TC] VP)
-- gather = gatherWith keyToValue


keyToValue :: [TC] -> VP
keyToValue = VPString . concatMap tcTyN

valueToKey :: VP -> [TC]
valueToKey = pure . mkTyN . show

