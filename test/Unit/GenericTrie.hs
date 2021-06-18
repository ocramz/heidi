module Unit.GenericTrie where

import Test.Tasty.Hspec (Spec, it, shouldBe)

import Heidi.Data.Frame.Algorithms.GenericTrie (innerJoin, leftOuterJoin, groupBy)
import Heidi (Frame, frame, Row, rowFromList)

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

employee :: Frame (Row String String)
employee = frame mempty [e1, e2, e3, e4, e5, e6] where
  e1 = rowFromList [("name", "Rafferty"), ("id.dep", "31")]
  e2 = rowFromList [("name", "Jones"), ("id.dep", "33")]
  e3 = rowFromList [("name", "Heisenberg"), ("id.dep", "33")]
  e4 = rowFromList [("name", "Robinson"), ("id.dep", "34")]
  e5 = rowFromList [("name", "Smith"), ("id.dep", "34")]
  e6 = rowFromList [("name", "Williams")]  

department :: Frame (Row String String)
department = frame mempty [d1, d2, d3, d4] where
  d1 = rowFromList [("id.dep", "31"), ("dept", "Sales")]
  d2 = rowFromList [("id.dep", "33"), ("dept", "Engineering")]
  d3 = rowFromList [("id.dep", "34"), ("dept", "Clerical")]
  d4 = rowFromList [("id.dep", "35"), ("dept", "Marketing")]  






t0 :: Frame (Row String String)
t0 = frame mempty [ book1, ball, bike, book2 ]
           where
             book1 = rowFromList [("item", "book"), ("id.0", "129"), ("qty", "1")]
             book2 = rowFromList [("item", "book"), ("id.0", "129"), ("qty", "5")]  
             ball = rowFromList [("item", "ball"), ("id.0", "234"), ("qty", "1")]  
             bike = rowFromList [("item", "bike"), ("id.0", "410"), ("qty", "1")]

t1 :: Frame (Row String String)
t1 = frame mempty [ r1, r2, r3, r4 ] 
           where
             r1 = rowFromList [("id.1", "129"), ("price", "100")]
             r2 = rowFromList [("id.1", "234"), ("price", "50")]  
             r3 = rowFromList [("id.1", "3"), ("price", "150")]
             r4 = rowFromList [("id.1", "99"), ("price", "30")]
