{-# language LambdaCase #-}
module Heidi.Data.Frame.Algorithms.GenericTrie.Generic where

-- containers
import qualified Data.Set as S
-- generic-trie
import qualified Data.GenericTrie as GT
-- text
import qualified Data.Text as T (Text, pack, unpack)

import Heidi.Data.Row.GenericTrie (Row(..))
import Heidi.Data.Frame.Algorithms.GenericTrie (spreadWith, gatherWith)
import Core.Data.Frame.List (Frame(..))
-- import Data.Generics.Codec ()

import Data.Generics.Encode.Internal (VP(..), TC(..), tcTyCon, tcTyN, mkTyCon, mkTyN)

{-
λ> :t spreadWith valueToKey

<interactive>:1:1-21: error:
    • Could not deduce (Ord VP) arising from a use of ‘spreadWith’
        bound by the inferred type of
                   it :: Foldable t =>
                         [TC] -> [TC] -> t (Row [TC] VP) -> Frame (Row [TC] VP)
-}


keyToValue :: [TC] -> VP
keyToValue = VPString . concatMap tcTyN

valueToKey :: VP -> [TC]
valueToKey = pure . mkTyN . show

