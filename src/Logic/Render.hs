{-# LANGUAGE TypeOperators #-}
module Logic.Render ( pretty
                    , Render(..)
                    ) where

import Logic.Formula

import Data.Text (Text)

class Render f where
  render :: (Show a, Render g) => f a (Fix (g a)) -> Text

pretty :: (Show a, Render f) => Formula f a -> Text
pretty = render . unFix . unFormula

instance (Render f, Render g) => Render (f :+: g) where
  render (Inl x) = render x
  render (Inr x) = render x
