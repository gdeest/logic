{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Text (Text)
import qualified Data.Text as T

class Render f where
  render :: (Render g) => f (Formula g) -> Text

pretty :: Render f => Formula f -> Text
pretty (Fix x) = render x

newtype AtomId = AtomId Int  deriving (Eq, Show)
newtype VarId  = VarId Int   deriving (Eq, Show)

data Propositional e = Atom AtomId | PTrue | PFalse | PAnd e e | POr e e | PNeg e
  deriving (Functor, Foldable)

instance Render Propositional where
  render (Atom id) = T.pack $ show id
  render PTrue  = "TRUE"
  render PFalse = "FALSE"
  render (PAnd (Fix e1) (Fix e2)) = T.concat ["(", render e1, ") /\\ (", render e2, ")"]
  render (POr  (Fix e1) (Fix e2)) = T.concat ["(", render e1, ") \\/ (", render e1, ")"]
  render (PNeg (Fix e)) = T.concat ["~(", render e, ")"]

data Implications e = Impl e e | Iff e e
  deriving (Functor, Foldable)

instance Render Implications where
  render (Impl (Fix e1) (Fix e2)) = T.concat ["(", render e1, ") -> (", render e2, ")"]
  render (Iff  (Fix e1) (Fix e2)) = T.concat ["(", render e1, ") <-> (", render e2, ")"]

data Quantifiers e = Forall VarId e | Exists VarId e
  deriving (Functor, Foldable, Show)

data Fix f = Fix { unFix :: f (Fix f) }
type Formula f = Fix f

data (:+:) f g x = Inl (f x) | Inr (g x)
  deriving (Functor, Foldable, Show)

instance (Render f, Render g) => Render (f :+: g) where
  render (Inl x) = render x
  render (Inr x) = render x

infixl 6 :+:

class Functor f => ToProp f where
  toProp' :: (ToProp f') => f (Fix f') -> Formula Propositional

instance ToProp Propositional where
  toProp' = Fix . fmap (toProp' . unFix)

instance ToProp Implications where
  toProp' = Fix . \case
    Impl e1 e2 -> POr (Fix $ PNeg (toProp' $ unFix e1)) (toProp' $ unFix e2)
    Iff e1 e2  -> PAnd (toProp' $ Impl e1 e2) (toProp' $ Impl e2 e1)

instance (ToProp f1, ToProp f2) => ToProp (f1 :+: f2) where
  toProp' (Inl e) = toProp' e
  toProp' (Inr e) = toProp' e

toProp :: ToProp f => Formula f -> Formula Propositional
toProp = toProp' . unFix

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance {-# OVERLAPPABLE #-} (Functor f) => f :<: f where
  inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: h) => f :<: (g :+: h) where
  inj = Inr . inj

inject :: (g :<: f) => g (Formula f) -> Formula f
inject = Fix . inj

ptrue, pfalse :: Propositional :<: f => Formula f
ptrue  = inject PTrue
pfalse = inject PFalse

por, pand :: Propositional :<: f => Formula f -> Formula f -> Formula f
por e1 e2 = inject (POr e1 e2)
pand e1 e2 = inject (PAnd e1 e2)

impl e1 e2 = inject (Impl e1 e2)

formula1 :: Formula (Propositional :+: Implications)
formula1 = impl (por ptrue pfalse) (impl ptrue ptrue)

