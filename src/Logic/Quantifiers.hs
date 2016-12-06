{-# LANGUAGE TemplateHaskell#-}
module Logic.Quantifiers ( Quantifiers(..)
                         ) where

import Logic.Eval
import Logic.Formula (VarId)
import Logic.Render

import Data.Bifunctor.TH

data Quantifiers a e = Forall VarId e | Exists VarId e
$(deriveBifunctor ''Quantifiers)
$(deriveBifoldable ''Quantifiers)
$(deriveBitraversable ''Quantifiers)
