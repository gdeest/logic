module Main where

import Lib

main :: IO ()
main =  do
  return ()
  print $ pretty formula1
  print $ pretty $ toProp formula1
