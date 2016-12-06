{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Logic.Implication ( impl
                         , Implication(..)
                         ) where

import Logic.Eval
import Logic.Formula
import Logic.Propositional
import Logic.Render
import Logic.ToProp

import Data.Bifunctor.TH
import qualified Data.Text as T

data Implication a e = Impl e e | Iff e e
$(deriveBifunctor ''Implication)
$(deriveBifoldable ''Implication)
$(deriveBitraversable ''Implication)

impl :: (Implication :<: f) => Formula f a -> Formula f a -> Formula f a
impl e1 e2 = inject (Impl (unFormula e1) (unFormula e2))

instance Render Implication where
  render (Impl (Fix e1) (Fix e2)) = T.concat ["(", render e1, ") -> (", render e2, ")"]
  render (Iff  (Fix e1) (Fix e2)) = T.concat ["(", render e1, ") <-> (", render e2, ")"]

instance Eval Implication where
  eval (Impl (Fix e1) (Fix e2)) val = pure (\a b -> (not a) || b) <*> (eval e1 val) <*> (eval e2 val)
  eval (Iff (Fix e1) (Fix e2)) val = pure (\a b -> ((not a) && (not b)) || (a && b)) <*> (eval e1 val) <*> (eval e2 val)

instance ToProp Implication where
  toProp' = Fix . \case
    Impl e1 e2 -> POr (Fix $ PNeg (toProp' $ unFix e1)) (toProp' $ unFix e2)
    Iff e1 e2  -> PAnd (toProp' $ Impl e1 e2) (toProp' $ Impl e2 e1)
