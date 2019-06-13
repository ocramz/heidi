module Unit.GenericTrie where

import Test.Tasty.Hspec (Spec, it, shouldBe)

import qualified Heidi.Data.Row.GenericTrie as GTR (Row, fromList)
import Heidi.Data.Frame.Algorithms.GenericTrie (innerJoin, leftOuterJoin, groupBy)
import Heidi (Frame, fromList)

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

employee :: Frame (GTR.Row String String)
employee = fromList [e1, e2, e3, e4, e5, e6] where
  e1 = GTR.fromList [("name", "Rafferty"), ("id.dep", "31")]
  e2 = GTR.fromList [("name", "Jones"), ("id.dep", "33")]
  e3 = GTR.fromList [("name", "Heisenberg"), ("id.dep", "33")]
  e4 = GTR.fromList [("name", "Robinson"), ("id.dep", "34")]
  e5 = GTR.fromList [("name", "Smith"), ("id.dep", "34")]
  e6 = GTR.fromList [("name", "Williams")]  

department :: Frame (GTR.Row String String)
department = fromList [d1, d2, d3, d4] where
  d1 = GTR.fromList [("id.dep", "31"), ("dept", "Sales")]
  d2 = GTR.fromList [("id.dep", "33"), ("dept", "Engineering")]
  d3 = GTR.fromList [("id.dep", "34"), ("dept", "Clerical")]
  d4 = GTR.fromList [("id.dep", "35"), ("dept", "Marketing")]  






t0 :: Frame (GTR.Row String String)
t0 = fromList [ book1, ball, bike, book2 ]
           where
             book1 = GTR.fromList [("item", "book"), ("id.0", "129"), ("qty", "1")]
             book2 = GTR.fromList [("item", "book"), ("id.0", "129"), ("qty", "5")]  
             ball = GTR.fromList [("item", "ball"), ("id.0", "234"), ("qty", "1")]  
             bike = GTR.fromList [("item", "bike"), ("id.0", "410"), ("qty", "1")]

t1 :: Frame (GTR.Row String String)
t1 = fromList [ r1, r2, r3, r4 ] 
           where
             r1 = GTR.fromList [("id.1", "129"), ("price", "100")]
             r2 = GTR.fromList [("id.1", "234"), ("price", "50")]  
             r3 = GTR.fromList [("id.1", "3"), ("price", "150")]
             r4 = GTR.fromList [("id.1", "99"), ("price", "30")]
