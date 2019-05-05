module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest [
  "src/Data/Generics/Encode/OneHot.hs",
  "src/Core/Data/Frame/Generic.hs",
  "src/Core/Data/Frame.hs"
  ]
