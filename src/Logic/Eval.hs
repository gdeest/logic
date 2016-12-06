{-# LANGUAGE TypeOperators #-}
module Logic.Eval ( evaluate
                  , satisfiable
                  , tautology
                  , Eval(..)
                  ) where

import Logic.Formula

import Data.Bifoldable
import Data.Maybe (fromJust)
import qualified Data.Map as M

class Eval f where
  eval :: (Eval g) => f a (Fix (g a)) -> (a -> Maybe Bool) -> Maybe Bool

evaluate :: (Eval f) => Formula f a -> (a -> Maybe Bool) -> Maybe Bool
evaluate = eval . unFix . unFormula


instance (Eval f1, Eval f2) => Eval (f1 :+: f2) where
  eval (Inl e) val = eval e val
  eval (Inr e) val = eval e val

tautology :: (Bifoldable f, Eval f, Ord a) => Formula f a -> Bool
tautology fml = foldr (\v a -> (&&) a (fromJust $ evaluate fml v)) True (map (flip M.lookup) $ valuations fml)

satisfiable :: (Bifoldable f, Eval f, Ord a) => Formula f a -> Maybe (M.Map a Bool)
satisfiable fml = case satisfyingValuations of
  (v:vs) -> Just v
  _      -> Nothing
  where satisfyingValuations = filter (fromJust . evaluate fml . flip M.lookup) $ valuations fml
