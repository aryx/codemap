module Main where

import Data.List

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

data Color = Red | Green | Blue

type Name = String

main :: IO ()
main = do
  let x = 42
  let msg = "hello"
  print (factorial x)
