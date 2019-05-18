module Main where
-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.

-- import Test.Tasty (testGroup, defaultMain)
import Test.Tasty (defaultMain)


-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.

-- import Test.Tasty.Hspec (Spec, testSpec, parallel, it, shouldBe)
import Test.Tasty.Hspec (Spec, testSpec, parallel)

import Unit

main :: IO ()
main = do
    test <- testSpec "Core.Data.Frame" spec_Frame
    defaultMain test

spec_Frame :: Spec
spec_Frame = parallel $ do
  test_innerJoin
  test_leftOuterJoin
  test_groupBy

-- spec :: Spec
-- spec = parallel $ do
--     it "is trivially true" $ do
--         True `shouldBe` True
