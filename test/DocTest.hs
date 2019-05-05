module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest [
  "src/Data/Generics/Encode/OneHot.hs",
  "src/Data/Generics/Encode/Val2.hs",  
  "src/Core/Data/Frame/Generic.hs",
  "src/Core/Data/Frame.hs"
  ]
