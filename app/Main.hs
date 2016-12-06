{-# LANGUAGE TypeOperators #-}
module Main where

import Logic

import Prelude hiding (putStrLn)
import Data.Foldable
import Data.Text.IO (putStrLn)

formula1 :: Formula (Propositional :+: Implication) Int
formula1 = impl (por ptrue pfalse) (impl ptrue (patom 2))

formula2 :: Formula Propositional Int
formula2 = pfalse

formula3 :: Formula Propositional Int
formula3 = (patom 1) `por` ptrue

main :: IO ()
main =  do
  return ()
  putStrLn $ pretty formula1
  putStrLn $ pretty $ toProp formula1
  putStrLn $ pretty $ fmap (+2) formula1
  print $ toList formula1
  print $ atoms formula1
  print $ tautology formula1
  print $ tautology formula2
  print $ tautology formula3
  print $ satisfiable formula3
