{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.TH
import Data.Bifunctor.Wrapped
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S

data Fix f = Fix { unFix :: f (Fix f) }
newtype Formula f a = Formula { unFormula :: Fix (f a) }

class Render f where
  render :: (Show a, Render g) => f a (Fix (g a)) -> Text

class Eval f where
  eval :: (Eval g) => f a (Fix (g a)) -> (a -> Maybe Bool) -> Maybe Bool

pretty :: (Show a, Render f) => Formula f a -> Text
pretty = render . unFix . unFormula

evaluate :: (Eval f) => Formula f a -> (a -> Maybe Bool) -> Maybe Bool
evaluate = eval . unFix . unFormula

newtype VarId  = VarId Int   deriving (Eq, Show)

data Propositional a e = Atom a | PTrue | PFalse | PAnd e e | POr e e | PNeg e
$(deriveBifunctor ''Propositional)
$(deriveBifoldable ''Propositional)
$(deriveBitraversable ''Propositional)

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

data Implications a e = Impl e e | Iff e e
$(deriveBifunctor ''Implications)
$(deriveBifoldable ''Implications)
$(deriveBitraversable ''Implications)

instance Render Implications where
  render (Impl (Fix e1) (Fix e2)) = T.concat ["(", render e1, ") -> (", render e2, ")"]
  render (Iff  (Fix e1) (Fix e2)) = T.concat ["(", render e1, ") <-> (", render e2, ")"]

instance Eval Implications where
  eval (Impl (Fix e1) (Fix e2)) val = pure (\a b -> (not a) || b) <*> (eval e1 val) <*> (eval e2 val)
  eval (Iff (Fix e1) (Fix e2)) val = pure (\a b -> ((not a) && (not b)) || (a && b)) <*> (eval e1 val) <*> (eval e2 val)

data Quantifiers a e = Forall VarId e | Exists VarId e
$(deriveBifunctor ''Quantifiers)
$(deriveBifoldable ''Quantifiers)
$(deriveBitraversable ''Quantifiers)

data (:+:) f g a e = Inl (f a e) | Inr (g a e)
infixl 6 :+:
$(deriveBifunctor ''(:+:))
$(deriveBifoldable ''(:+:))
$(deriveBitraversable ''(:+:))

instance (Render f, Render g) => Render (f :+: g) where
  render (Inl x) = render x
  render (Inr x) = render x


class Bifunctor f => ToProp f where
  toProp' :: (ToProp g) => f a (Fix (g a)) -> Fix (Propositional a)

toProp :: ToProp f => Formula f a -> Formula Propositional a
toProp = Formula . toProp' . unFix . unFormula

instance ToProp Propositional where
  toProp' = Fix . unwrapBifunctor . fmap (toProp' . unFix) . WrapBifunctor
  {-# NOINLINE toProp'#-}

instance ToProp Implications where
  toProp' = Fix . \case
    Impl e1 e2 -> POr (Fix $ PNeg (toProp' $ unFix e1)) (toProp' $ unFix e2)
    Iff e1 e2  -> PAnd (toProp' $ Impl e1 e2) (toProp' $ Impl e2 e1)

instance (ToProp f1, ToProp f2) => ToProp (f1 :+: f2) where
  toProp' (Inl e) = toProp' e
  toProp' (Inr e) = toProp' e

instance (Eval f1, Eval f2) => Eval (f1 :+: f2) where
  eval (Inl e) val = eval e val
  eval (Inr e) val = eval e val


class (Bifunctor sub, Bifunctor sup) => sub :<: sup where
  inj :: sub a e -> sup a e

instance {-# OVERLAPPABLE #-} (Bifunctor f) => f :<: f where
  inj = id

instance (Bifunctor f, Bifunctor g) => f :<: (f :+: g) where
  inj = Inl

instance {-# OVERLAPPABLE #-} (Bifunctor f, Bifunctor g, Bifunctor h, f :<: h) => f :<: (g :+: h) where
  inj = Inr . inj

inject :: (g :<: f) => g a (Fix (f a)) -> Formula f a
inject = Formula . Fix . inj

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

impl :: (Implications :<: f) => Formula f a -> Formula f a -> Formula f a
impl e1 e2 = inject (Impl (unFormula e1) (unFormula e2))

instance (Bifunctor f) => Functor (Formula f) where
  fmap f = Formula . (liftFix f) . unFormula
    where liftFix f = Fix . bimap f (liftFix f) . unFix

instance (Bifoldable f) => Foldable (Formula f) where
  foldMap f = (bifoldMap f (foldMap f . Formula)) . unFix . unFormula

atoms :: (Bifoldable f, Ord a) => Formula f a -> S.Set a
atoms = foldr S.insert S.empty

valuations :: (Bifoldable f, Ord a) => Formula f a -> [M.Map a Bool]
valuations = foldM f M.empty . atoms
  where f m k = map (\v -> M.insert k v m) [True, False]

tautology :: (Bifoldable f, Eval f, Ord a) => Formula f a -> Bool
tautology fml = foldr (\v a -> (&&) a (fromJust $ evaluate fml v)) True (map (flip M.lookup) $ valuations fml)

safeHead (x:xs) = Just x
safeHead _ = Nothing

satisfiable :: (Bifoldable f, Eval f, Ord a) => Formula f a -> Maybe (M.Map a Bool)
satisfiable fml = safeHead . filter (fromJust . evaluate fml . flip M.lookup) $ valuations fml
