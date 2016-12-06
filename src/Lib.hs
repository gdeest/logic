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























safeHead (x:xs) = Just x
safeHead _ = Nothing

