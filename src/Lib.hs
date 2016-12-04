{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Bifunctor
import Data.Bifunctor.TH
import Data.Bifunctor.Wrapped
import Data.Text (Text)
import qualified Data.Text as T

data Fix f = Fix { unFix :: f (Fix f) }
type Formula f a = Fix (f a)

class Render f where
  render :: (Show a, Render g) => f a (Formula g a) -> Text

pretty :: (Show a, Render f) => Formula f a -> Text
pretty (Fix x) = render x

newtype VarId  = VarId Int   deriving (Eq, Show)

data Propositional a e = Atom a | PTrue | PFalse | PAnd e e | POr e e | PNeg e
$(deriveBifunctor ''Propositional)

instance Render Propositional where
  render (Atom id) = T.pack $ show id
  render PTrue  = "TRUE"
  render PFalse = "FALSE"
  render (PAnd (Fix e1) (Fix e2)) = T.concat ["(", render e1, ") /\\ (", render e2, ")"]
  render (POr  (Fix e1) (Fix e2)) = T.concat ["(", render e1, ") \\/ (", render e1, ")"]
  render (PNeg (Fix e)) = T.concat ["~(", render e, ")"]

data Implications a e = Impl e e | Iff e e
$(deriveBifunctor ''Implications)

instance Render Implications where
  render (Impl (Fix e1) (Fix e2)) = T.concat ["(", render e1, ") -> (", render e2, ")"]
  render (Iff  (Fix e1) (Fix e2)) = T.concat ["(", render e1, ") <-> (", render e2, ")"]

data Quantifiers a e = Forall VarId e | Exists VarId e
$(deriveBifunctor ''Quantifiers)

data (:+:) f g a e = Inl (f a e) | Inr (g a e)

instance (Bifunctor f, Bifunctor g) => Bifunctor (f :+: g) where
  bimap f g (Inl fa) = Inl $ bimap f g fa
  bimap f g (Inr ga) = Inr $ bimap f g ga

instance (Render f, Render g) => Render (f :+: g) where
  render (Inl x) = render x
  render (Inr x) = render x

infixl 6 :+:

class Bifunctor f => ToProp f where
  toProp' :: (ToProp g) => f a (Fix (g a)) -> Formula Propositional a

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

toProp :: ToProp f => Formula f a -> Formula Propositional a
toProp = toProp' . unFix

class (Bifunctor sub, Bifunctor sup) => sub :<: sup where
  inj :: sub a e -> sup a e

instance {-# OVERLAPPABLE #-} (Bifunctor f) => f :<: f where
  inj = id

instance (Bifunctor f, Bifunctor g) => f :<: (f :+: g) where
  inj = Inl

instance {-# OVERLAPPABLE #-} (Bifunctor f, Bifunctor g, Bifunctor h, f :<: h) => f :<: (g :+: h) where
  inj = Inr . inj

inject :: (g :<: f) => g a (Formula f a) -> Formula f a
inject = Fix . inj

ptrue, pfalse :: (Propositional :<: f) => Formula f a
ptrue  = inject PTrue
pfalse = inject PFalse

por, pand :: (Propositional :<: f) => Formula f a -> Formula f a -> Formula f a
por e1 e2 = inject (POr e1 e2)
pand e1 e2 = inject (PAnd e1 e2)

impl e1 e2 = inject (Impl e1 e2)

formula1 :: Formula (Propositional :+: Implications) Int
formula1 = impl (por ptrue pfalse) (impl ptrue ptrue)

-- instance (Bifunctor f) => Functor (Fix f) where
--   fmap f (Fix x) = undefined
