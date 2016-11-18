----------------------------------------------------------------------------
-- |
-- Module      :  Data.MsgId
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  27 November 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.MsgId
  ( MsgId
  , unMsgId
  , initMsgId
  , incrementMsgId
  ) where

import Data.Binary (Binary)
import Data.Coerce
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | Non-negative message number.
newtype MsgId = MsgId { unMsgId :: Int }
  deriving (Eq, Ord, Generic, Typeable, Binary)

instance Show MsgId where
  show = show . unMsgId

initMsgId :: MsgId
initMsgId = MsgId 0

incrementMsgId :: MsgId -> MsgId
incrementMsgId = coerce ((+1) :: Int -> Int)
