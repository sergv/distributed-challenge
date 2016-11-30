----------------------------------------------------------------------------
-- |
-- Module      :  Challenge.Node
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday, 24 November 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Challenge.Node
  ( runNode
  , sendToNode
  , terminateNode
  ) where

import Control.Monad
import Data.Binary (Binary)
import Data.Typeable
import GHC.Generics (Generic)
import System.IO

import Control.Distributed.Process.Ext
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable

import Challenge.Types

newtype TerminateNode = TerminateNode ProcessId
  deriving (Show, Generic, Typeable)

instance Binary TerminateNode

nodeKeeperId :: String
nodeKeeperId = "nodeKeeper"

-- | Request a node to cease operating.
terminateNode :: NodeId -> Process ()
terminateNode node = nsendRemote node nodeKeeperId . TerminateNode =<< getSelfPid

-- | Send a message to node keeper process.
sendToNode :: Serializable a => NodeId -> a -> Process ()
sendToNode node = nsendRemote node nodeKeeperId

-- | Signal to logger process to stop.
data LoggerShutdown = LoggerShutdown ProcessId
  deriving (Show, Eq, Ord, Generic, Typeable)

instance Binary LoggerShutdown

data LoggerShutdownStarted = LoggerShutdownStarted
  deriving (Show, Eq, Ord, Generic, Typeable)

instance Binary LoggerShutdownStarted

addLogger :: (?debugLevel :: DebugLevel) => Process ProcessId
addLogger = do
  newLoggerPid <- spawnLocal logRequests
  loggerPid <- whereis "logger"
  case loggerPid of
    Nothing -> register "logger" newLoggerPid
    Just _  -> reregister "logger" newLoggerPid
  pure newLoggerPid
  where
    logRequests = do
      continue <- receiveWait
        [ match $ \(_time :: String, pid :: ProcessId, msg :: String) -> do
            liftIO $ hPutStrLnDebug stderr $ show pid ++ ": " ++ msg
            pure True
        , match $ \(LoggerShutdown requester) -> do
            liftIO $ hFlush stderr
            send requester LoggerShutdownStarted
            pure False
        , matchAny $ \msg -> do
            sayDebug $ "[node logger] ignoring message: " ++ show msg
            pure True
        ]
      when continue
        logRequests

-- | Wait until logger logs out all its messages.
flushLogger :: ProcessId -> Process ()
flushLogger loggerPid = do
  send loggerPid . LoggerShutdown =<< getSelfPid
  -- Wait until logger acknowledges our shutdown request.
  LoggerShutdownStarted <- expect
  pure ()

nodeKeeperProcess
  :: (Show a, Serializable a, ?debugLevel :: DebugLevel)
  => (ReceivePort a -> Process ())
  -> Process ()
nodeKeeperProcess worker = do
  loggerPid <- addLogger
  nid       <- getSelfNode
  pid       <- getSelfPid
  sayDebug $ "Started node keeper on node " ++ show nid ++ " with pid " ++ show pid
  register nodeKeeperId pid
  (forwardsSink, forwardsSource) <- newChan
  void $ spawnLocal $ worker forwardsSource
  loop forwardsSink
  flushLogger loggerPid
  where
    loop forwardsSink = go
      where
        go = do
          continue <- receiveWait
            [ match $ \msg -> do
                sayDebug $ "[nodeKeeper] forwarding message to worker: " ++ show msg
                sendChan forwardsSink msg
                pure True
            , match $ \msg@(TerminateNode _) -> do
                sayDebug $ "[nodeKeeper] terminating node by request " ++ show msg
                pure False
            , matchAny $ \msg -> do
                sayDebug $ "[nodeKeeper] ignoring message: " ++ show msg
                pure True
            ]
          when continue
            go

runNode
  :: (Show a, Serializable a, ?debugLevel :: DebugLevel)
  => RemoteTable
  -> EndPointConfig
  -> (ReceivePort a -> Process ())
  -> IO ()
runNode remoteTable cfg worker = do
  t    <- makeTransport cfg
  node <- newLocalNode t remoteTable
  runProcess node $ nodeKeeperProcess worker

