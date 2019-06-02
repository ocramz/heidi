module Unit where

import Test.Tasty.Hspec (Spec, it, shouldBe)

import qualified Heidi.Data.Row.HashMap as HMR (Row, fromList)
import Heidi (Frame, fromList, groupBy, innerJoin, leftOuterJoin)


test_innerJoin :: Spec
test_innerJoin = it "innerJoin" $ do
  let jt = innerJoin "id.dep" "id.dep" employee department
  length jt `shouldBe` 5

test_leftOuterJoin :: Spec
test_leftOuterJoin = it "leftOuterJoin" $ do
  let jt = leftOuterJoin "id.dep" "id.dep" employee department
  length jt `shouldBe` 6  

test_groupBy :: Spec
test_groupBy = it "groupBy" $ do
  let gt = groupBy "id.dep" employee
  length gt `shouldBe` 3



-- example data from https://en.wikipedia.org/wiki/Join_(SQL)

employee :: Frame (HMR.Row String String)
employee = fromList [e1, e2, e3, e4, e5, e6] where
  e1 = HMR.fromList [("name", "Rafferty"), ("id.dep", "31")]
  e2 = HMR.fromList [("name", "Jones"), ("id.dep", "33")]
  e3 = HMR.fromList [("name", "Heisenberg"), ("id.dep", "33")]
  e4 = HMR.fromList [("name", "Robinson"), ("id.dep", "34")]
  e5 = HMR.fromList [("name", "Smith"), ("id.dep", "34")]
  e6 = HMR.fromList [("name", "Williams")]  

department :: Frame (HMR.Row String String)
department = fromList [d1, d2, d3, d4] where
  d1 = HMR.fromList [("id.dep", "31"), ("dept", "Sales")]
  d2 = HMR.fromList [("id.dep", "33"), ("dept", "Engineering")]
  d3 = HMR.fromList [("id.dep", "34"), ("dept", "Clerical")]
  d4 = HMR.fromList [("id.dep", "35"), ("dept", "Marketing")]  






t0 :: Frame (HMR.Row String String)
t0 = fromList [ book1, ball, bike, book2 ]
           where
             book1 = HMR.fromList [("item", "book"), ("id.0", "129"), ("qty", "1")]
             book2 = HMR.fromList [("item", "book"), ("id.0", "129"), ("qty", "5")]  
             ball = HMR.fromList [("item", "ball"), ("id.0", "234"), ("qty", "1")]  
             bike = HMR.fromList [("item", "bike"), ("id.0", "410"), ("qty", "1")]

t1 :: Frame (HMR.Row String String)
t1 = fromList [ r1, r2, r3, r4 ] 
           where
             r1 = HMR.fromList [("id.1", "129"), ("price", "100")]
             r2 = HMR.fromList [("id.1", "234"), ("price", "50")]  
             r3 = HMR.fromList [("id.1", "3"), ("price", "150")]
             r4 = HMR.fromList [("id.1", "99"), ("price", "30")]


