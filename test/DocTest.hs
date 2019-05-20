module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest [
  "src/Data/Generics/Encode/OneHot.hs",
  "src/Data/Generics/Encode/Internal.hs",  
  "src/Core/Data/Frame/Generic.hs",
  "src/Core/Data/Frame.hs",
  "src/Core/Data/Row/HashMap.hs"
  ]
