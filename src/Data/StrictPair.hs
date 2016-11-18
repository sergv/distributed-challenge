----------------------------------------------------------------------------
-- |
-- Module      :  Data.StrictPair
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  30 November 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Data.StrictPair (StrictPair(..)) where

import Data.Monoid

data StrictPair a b = StrictPair !a !b
  deriving (Show, Eq, Ord)

instance (Monoid a, Monoid b) => Monoid (StrictPair a b) where
  mempty = StrictPair mempty mempty
  mappend (StrictPair x y) (StrictPair x' y') = StrictPair (x <> x') (y <> y')
