----------------------------------------------------------------------------
-- |
-- Module      :  Data.RandomNumber
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  29 November 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.RandomNumber
  ( RandomNumber
  , unRandomNumber
  , genRandomNumber
  ) where

import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import System.Random

-- Random real from range (0, 1].
newtype RandomNumber = RandomNumber { unRandomNumber :: Double }
  deriving (Show, Eq, Ord, Generic, Typeable, Binary)

genRandomNumber :: RandomGen g => g -> (RandomNumber, g)
genRandomNumber g = (RandomNumber $! 1 - x, g')
  where
    (x, g') = random g

