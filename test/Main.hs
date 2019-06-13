module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (Spec, testSpec, parallel)

import qualified Unit.HashMap as UHMR
import qualified Unit.GenericTrie as UGTR

main :: IO ()
main = do
    test_hmr <- testSpec "Heidi.Data.Frame.Algorithms.HashMap" spec_Frame_HMR
    test_gtr <- testSpec "Heidi.Data.Frame.Algorithms.GenericTrie" spec_Frame_GTR
    defaultMain $ testGroup "HMR" [test_hmr, test_gtr]

spec_Frame_HMR :: Spec
spec_Frame_HMR = parallel $ do
  UHMR.test_innerJoin
  UHMR.test_leftOuterJoin
  UHMR.test_groupBy
  
spec_Frame_GTR :: Spec
spec_Frame_GTR = parallel $ do  
  UGTR.test_innerJoin
  UGTR.test_leftOuterJoin
  UGTR.test_groupBy  

-- spec :: Spec
-- spec = parallel $ do
--     it "is trivially true" $ do
--         True `shouldBe` True
