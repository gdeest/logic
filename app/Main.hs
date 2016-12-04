module Main where

import Lib

main :: IO ()
main =  do
  print $ pretty formula1
  print $ pretty $ toProp formula1
