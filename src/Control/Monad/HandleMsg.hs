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
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Control.Monad.HandleMsg
  ( HandleMsgM
  , sendTo
  , runHandleMsgM
  , debugLog
  ) where

import Control.Distributed.Process.Ext
import Control.Distributed.Process.Serializable
import Control.Monad.RWS
import Data.Foldable

data SendRequest = forall a. (Show a, Serializable a) =>
  SendRequest a (SendPort a)

deriving instance Show SendRequest

newtype HandleMsgM r s a = HandleMsgM (RWS r ([SendRequest], [String]) s a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader r
    , MonadState s
    )

sendTo :: (Show a, Serializable a) => a -> SendPort a -> HandleMsgM r s ()
sendTo msg dest = HandleMsgM $ tell ([SendRequest msg dest], mempty)

debugLog :: String -> HandleMsgM r s ()
debugLog msg = HandleMsgM $ tell (mempty, [msg])

runHandleMsgM
  :: (?debugLevel :: DebugLevel)
  => r
  -> s
  -> HandleMsgM r s () -> Process s
runHandleMsgM r s (HandleMsgM action) = do
  let ((), s', (msgs, traces)) = runRWS action r s
  for_ msgs $ \(SendRequest msg dest) -> do
    say $ "[runHandleMsgM] sending: " ++ show msg ++ " to " ++ show (sendPortId dest)
    sendChan dest msg
  for_ traces sayDebug
  pure s'
