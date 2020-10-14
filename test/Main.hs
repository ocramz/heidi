module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (Spec, testSpec, parallel)

import qualified Unit.GenericTrie as UGTR

main :: IO ()
main = do
    test_gtr <- testSpec "GenericTrie-based rows" spec_Frame_GTR
    defaultMain $ do
      testGroup "Heidi.Data.Frame.Algorithms" [test_gtr]

spec_Frame_GTR :: Spec
spec_Frame_GTR = parallel $ do
  UGTR.test_innerJoin
  UGTR.test_leftOuterJoin
  UGTR.test_groupBy

-- spec :: Spec
-- spec = parallel $ do
--     it "is trivially true" $ do
--         True `shouldBe` True
