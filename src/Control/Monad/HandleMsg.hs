----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.HandleMsg
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

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Control.Monad.HandleMsg
  ( HandleMsgM
  , sendTo
  , runHandleMsgM
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Control.Monad.RWS
import Data.Foldable

data SendRequest = forall a. (Show a, Serializable a) =>
  SendRequest a (SendPort a)

deriving instance Show SendRequest

newtype HandleMsgM r s a = HandleMsgM (RWS r [SendRequest] s a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader r
    , MonadState s
    )

sendTo :: (Show a, Serializable a) => a -> SendPort a -> HandleMsgM r s ()
sendTo msg dest = HandleMsgM $ tell [SendRequest msg dest]

runHandleMsgM :: r -> s -> HandleMsgM r s () -> Process s
runHandleMsgM r s (HandleMsgM action) = do
  let ((), s', msgs) = runRWS action r s
  for_ msgs $ \(SendRequest msg dest) -> do
    say $ "[runHandleMsgM] sending: " ++ show msg ++ " to " ++ show dest
    sendChan dest msg
  pure s'
