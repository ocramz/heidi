module Unit where

import Test.Tasty.Hspec (Spec, it, shouldBe)

-- import Core.Data.Frame (Frame, Row, fromList, fromKVs, groupBy, innerJoin, leftOuterJoin)
import Core.Data.Frame (Frame, fromList, groupBy, innerJoin, leftOuterJoin)
import Core.Data.Row.HashMap (Row, fromKVs)


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
employee = fromList [e1, e2, e3, e4, e5, e6] where
  e1 = fromKVs [("name", "Rafferty"), ("id.dep", "31")]
  e2 = fromKVs [("name", "Jones"), ("id.dep", "33")]
  e3 = fromKVs [("name", "Heisenberg"), ("id.dep", "33")]
  e4 = fromKVs [("name", "Robinson"), ("id.dep", "34")]
  e5 = fromKVs [("name", "Smith"), ("id.dep", "34")]
  e6 = fromKVs [("name", "Williams")]  

department :: Frame (Row String String)
department = fromList [d1, d2, d3, d4] where
  d1 = fromKVs [("id.dep", "31"), ("dept", "Sales")]
  d2 = fromKVs [("id.dep", "33"), ("dept", "Engineering")]
  d3 = fromKVs [("id.dep", "34"), ("dept", "Clerical")]
  d4 = fromKVs [("id.dep", "35"), ("dept", "Marketing")]  






t0 :: Frame (Row String String)
t0 = fromList [ book1, ball, bike, book2 ]
           where
             book1 = fromKVs [("item", "book"), ("id.0", "129"), ("qty", "1")]
             book2 = fromKVs [("item", "book"), ("id.0", "129"), ("qty", "5")]  
             ball = fromKVs [("item", "ball"), ("id.0", "234"), ("qty", "1")]  
             bike = fromKVs [("item", "bike"), ("id.0", "410"), ("qty", "1")]

t1 :: Frame (Row String String)
t1 = fromList [ r1, r2, r3, r4 ] :: Frame (Row String String)
           where
             r1 = fromKVs [("id.1", "129"), ("price", "100")]
             r2 = fromKVs [("id.1", "234"), ("price", "50")]  
             r3 = fromKVs [("id.1", "3"), ("price", "150")]
             r4 = fromKVs [("id.1", "99"), ("price", "30")]


