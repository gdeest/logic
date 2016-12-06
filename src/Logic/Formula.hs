{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Logic.Formula ( (:+:)(..)
                     , (:<:)(..)
                     , inject
                     , atoms
                     , valuations
                     , Fix(..)
                     , Formula(..)
                     , VarId(..)
                 ) where

import Control.Monad (foldM)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.TH
import qualified Data.Set as S
import qualified Data.Map as M

newtype VarId  = VarId Int   deriving (Eq, Show)

data Fix f = Fix { unFix :: f (Fix f) }
newtype Formula f a = Formula { unFormula :: Fix (f a) }

data (:+:) f g a e = Inl (f a e) | Inr (g a e)
infixl 6 :+:
$(deriveBifunctor ''(:+:))
$(deriveBifoldable ''(:+:))
$(deriveBitraversable ''(:+:))

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
