{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Logic.Propositional ( patom
                           , ptrue, pfalse
                           , pand, por
                           , pneg
                           , Propositional(..)
                           ) where

import Logic.Eval
import Logic.Formula
import Logic.Render

import Data.Bifunctor.TH
import qualified Data.Text as T

data Propositional a e = Atom a | PTrue | PFalse | PAnd e e | POr e e | PNeg e
$(deriveBifunctor ''Propositional)
$(deriveBifoldable ''Propositional)
$(deriveBitraversable ''Propositional)

ptrue, pfalse :: (Propositional :<: f) => Formula f a
ptrue  = inject PTrue
pfalse = inject PFalse

por, pand :: (Propositional :<: f) => Formula f a -> Formula f a -> Formula f a
por e1 e2 = inject (POr (unFormula e1) (unFormula e2))
pand e1 e2 = inject (PAnd (unFormula e1) (unFormula e2))

pneg :: (Propositional :<: f) => Formula f a -> Formula f a
pneg = inject . PNeg . unFormula

patom :: (Propositional :<: f) => a -> Formula f a
patom = inject . Atom

instance Render Propositional where
  render (Atom id) = T.pack $ show id
  render PTrue  = "TRUE"
  render PFalse = "FALSE"
  render (PAnd (Fix e1) (Fix e2)) = T.concat ["(", render e1, ") /\\ (", render e2, ")"]
  render (POr  (Fix e1) (Fix e2)) = T.concat ["(", render e1, ") \\/ (", render e1, ")"]
  render (PNeg (Fix e)) = T.concat ["~(", render e, ")"]

instance Eval Propositional where
  eval (Atom id) val = val id
  eval PTrue _ = Just True
  eval PFalse _ = Just False
  eval (PAnd (Fix e1) (Fix e2)) val = pure (&&) <*> eval e1 val <*> eval e2 val
  eval (POr  (Fix e1) (Fix e2)) val = pure (||) <*> eval e1 val <*> eval e2 val
  eval (PNeg (Fix e)) val = pure (not) <*> eval e val
