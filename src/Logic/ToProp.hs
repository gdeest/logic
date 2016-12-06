{-# LANGUAGE TypeOperators #-}
module Logic.ToProp ( toProp
                       , ToProp(..)
                       ) where

import Logic.Formula
import Logic.Propositional

import Data.Bifunctor
import Data.Bifunctor.Wrapped

class Bifunctor f => ToProp f where
  toProp' :: (ToProp g) => f a (Fix (g a)) -> Fix (Propositional a)

toProp :: ToProp f => Formula f a -> Formula Propositional a
toProp = Formula . toProp' . unFix . unFormula

instance (ToProp f1, ToProp f2) => ToProp (f1 :+: f2) where
  toProp' (Inl e) = toProp' e
  toProp' (Inr e) = toProp' e

instance ToProp Propositional where
  toProp' = Fix . unwrapBifunctor . fmap (toProp' . unFix) . WrapBifunctor
  {-# NOINLINE toProp'#-}
