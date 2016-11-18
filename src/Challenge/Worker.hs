----------------------------------------------------------------------------
-- |
-- Module      :  Challenge.Worker
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

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DoAndIfThenElse            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Challenge.Worker
  ( worker
  , workerRemoteTable
  , WorkerConfig(..)
  , Microseconds(..)
  , fromSeconds
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Data.Binary (Binary)
import Data.Foldable
import qualified Data.Map as M
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.IO

import Control.Distributed.Process hiding (bracket)
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Serializable

import Challenge.Node
import Challenge.Worker.Types
import Control.Monad.HandleMsg
import Data.MsgId
import Data.RandomNumber
import Data.StrictPair

import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.Mtl

type HandleMsgM' = HandleMsgM WorkerEnv WorkerState

data MergedMessage =
    MMPresenceAnnouncement PresenceAnnouncement
  | MMPresenceAcknowledgement PresenceAcknowledgement
  | MMNodeDisconnected NodeDisconnected
  | MMRandomMessage RandomMessage
  | MMRandomMessageAcknowledgement RandomMessageAcknowledgement
  | MMSharePeerStateRequest SharePeerStateReq
  | MMSharePeerStateResponse SharePeerStateResp
  | MMTick Tick
  | MMSynchronizePeersTick SynchronizePeersTick
  | MMGracePeriodNotification GracePeriodNotification
  deriving (Show, Eq, Ord, Generic, Typeable)

instance Binary MergedMessage

data WorkerConfig = WorkerConfig
  { wcfgRandomSeed                      :: Int
  , wcfgSendInterval                    :: Microseconds
  , wcfgWaitInterval                    :: Microseconds
  , wcfgPeers                           :: Set NodeId
  , wcfgTickInterval                    :: Microseconds
  , wcfgPeerSynchronizationTickInterval :: Microseconds
  } deriving (Show, Eq, Ord)

newtype Microseconds = Microseconds { unMicroseconds :: Int }
  deriving (Show, Eq, Ord, Generic, Typeable)

fromSeconds :: Int -> Microseconds
fromSeconds = Microseconds .  (* 1000000)

-- | Generate 'RandomMessage' to be sent next. We may have sent it before, but
-- haven't got enough acknowledgemens to consider it received.
currentRandomMessage
  :: (MonadState WorkerState m, MonadReader WorkerEnv m)
  => m RandomMessage
currentRandomMessage = do
  ws <- get
  we <- ask
  pure RandomMessage
    { _rmRandomPayload = ws ^. wsUnacknowledgedRandomNum
    , _rmSender        = we ^. weWorkerPid
    , _rmMsgId         = ws ^. wsLocalCommState . csNextExpectedMsgId
    , _rmResponseSink  = we ^. weWorkerPrivatePorts . ppWorkerMessageAckSink
    }

prepareForSendingNextRandomMessage :: MonadState WorkerState m => CommunicationState -> m ()
prepareForSendingNextRandomMessage newCommunicationState = do
  (x, gen) <- genRandomNumber <$> use wsRandomGen
  wsRandomGen               .= gen
  wsUnacknowledgedRandomNum .= x
  wsLocalCommState          .= newCommunicationState

-- | Monitor nodes and send 'NodeDisconnected' message if any of them disconnects.
monitorNodes :: Traversable f => f NodeId -> SendPort NodeDisconnected -> Process ()
monitorNodes nodes msgSink =
  bracket (traverse monitorNode nodes) (traverse unmonitor) $ \_ ->
    forever $
      receiveWait
        [ match $ \(NodeMonitorNotification _ disconnectedNode _) -> do
            say $ "[monitorNodes] node disconnected: " ++ show disconnectedNode
            sendChan msgSink $ NodeDisconnected disconnectedNode
        , matchAny $ \msg ->
            say $ "[monitorNodes] ignoring message: " ++ show msg
        ]

sendTicks :: (Serializable a, Show a) => Microseconds -> (MsgId -> a) -> SendPort a -> Process ()
sendTicks delayInterval mkMsg msgSink = go initMsgId
  where
    go msgId = do
      liftIO $ threadDelay $ unMicroseconds delayInterval
      liftIO $ putStrLn $ "[sendTicks] sending tick " ++ show msgId
      sendChan msgSink $ mkMsg msgId
      go $! incrementMsgId msgId

notifyGracePeriod :: WorkerConfig -> SendPort GracePeriodNotification -> Process ()
notifyGracePeriod WorkerConfig{wcfgSendInterval, wcfgWaitInterval} sink = do
  liftIO $ threadDelay $ unMicroseconds wcfgSendInterval
  sendChan sink GracePeriodStarted
  liftIO $ threadDelay $ unMicroseconds wcfgWaitInterval
  sendChan sink GracePeriodEnded

reportResult :: MonadIO m => WorkerState -> m ()
reportResult WorkerState{_wsPeers, _wsLocalCommState} = liftIO $ do
  hPutStrLn stderr $
    "This node sent " ++
    show (nextExpectedMsgIdToMesagesReceived $ _csNextExpectedMsgId _wsLocalCommState) ++
    " acknowledged messages"
  putStrLn $ show (numberOfMessages, randomSum)
  where
    StrictPair (Sum numberOfMessages) (Sum randomSum) = mconcat
      [ resultFromCommunicationState _wsLocalCommState
      , foldMap (resultFromCommunicationState . _psCommunicationState) _wsPeers
      ]
    resultFromCommunicationState commState =
      StrictPair
        (Sum $ nextExpectedMsgIdToMesagesReceived $ commState ^. csNextExpectedMsgId)
        (Sum $ commState ^. csStateSum)
    nextExpectedMsgIdToMesagesReceived :: MsgId -> Int
    nextExpectedMsgIdToMesagesReceived msgId = max (unMsgId msgId - 1) 0

worker :: WorkerConfig -> ReceivePort PresenceAnnouncement -> Process ()
worker cfg@WorkerConfig{wcfgRandomSeed, wcfgPeers, wcfgTickInterval, wcfgPeerSynchronizationTickInterval} announcementsSource = do
  mypid <- getSelfPid
  let mynid = processNodeId mypid

  unless (processNodeId mypid `S.member` wcfgPeers) $
    error $ "Current node's id is not registered among peers: " ++ show wcfgPeers

  let peersToMonitor = toList $ mynid `S.delete` wcfgPeers

  (nodeDisconnectionsSink, nodeDisconnectionsSource) <- newChan
  void $ spawnLocal $ monitorNodes peersToMonitor nodeDisconnectionsSink

  (ticksSink, ticksSource) <- newChan
  void $ spawnLocal $ sendTicks wcfgTickInterval Tick ticksSink

  (synchronizeTicksSink, synchronizeTicksSource) <- newChan
  void $ spawnLocal $ sendTicks wcfgPeerSynchronizationTickInterval SynchronizePeersTick synchronizeTicksSink

  (gracePeriodSink, gracePeriodSource) <- newChan
  void $ spawnLocal $ notifyGracePeriod cfg gracePeriodSink

  (randomMsgSink,      randomMsgSource)      <- newChan
  (randomMsgAckSink,   randomMsgAckSource)   <- newChan
  (presenceAckSink,    presenceAckSource)    <- newChan
  (sharePeersSink,     sharePeersSource)     <- newChan
  (sharePeersRespSink, sharePeersReskSource) <- newChan
  let commPorts    = CommunicationPorts
        { _cpMessagesSink                 = randomMsgSink
        , _cpSharePeerStateSink           = sharePeersSink
        }
      privatePorts = PrivatePorts
        { _ppWorkerSharePeerStateRespSink = sharePeersRespSink
        , _ppWorkerMessageAckSink         = randomMsgAckSink
        }
  for_ peersToMonitor $ \peerNode ->
    sendToNode peerNode $ PresenceAnnouncement mypid presenceAckSink commPorts

  mergedChannel <- mergePortsRR
    [ MMPresenceAnnouncement         <$> announcementsSource
    , MMPresenceAcknowledgement      <$> presenceAckSource
    , MMNodeDisconnected             <$> nodeDisconnectionsSource

    , MMRandomMessage                <$> randomMsgSource
    , MMRandomMessageAcknowledgement <$> randomMsgAckSource

    , MMSharePeerStateRequest        <$> sharePeersSource
    , MMSharePeerStateResponse       <$> sharePeersReskSource

    , MMTick                         <$> ticksSource
    , MMSynchronizePeersTick         <$> synchronizeTicksSource
    , MMGracePeriodNotification      <$> gracePeriodSource
    ]

  let workerEnv :: WorkerEnv
      workerEnv = WorkerEnv
        { _weWorkerPid                = mypid
        , _weWorkerCommunicationPorts = commPorts
        , _weWorkerPrivatePorts       = privatePorts
        , _weTotalPeers               = S.size wcfgPeers
        , _weOrderAmongPeers          = myOrderAmongPeers
        }
      initWorkerState :: WorkerState
      (initWorkerState, myOrderAmongPeers) = mkWorkerState mynid wcfgPeers wcfgRandomSeed
      handleCommunication :: WorkerState -> Process WorkerState
      handleCommunication state =
        case state ^. wsCommunicationMode of
          Done -> do
            say "[worker.handleCommunication] Done"
            reportResult state
            terminateNode =<< getSelfNode
            pure state
          _    -> do
            msg <- receiveChan mergedChannel
            say $ "[worker.handleCommunication] got message " ++ show msg
            runHandleMsgM workerEnv state (handleMergedMessage msg) >>= handleCommunication
  void $ handleCommunication initWorkerState

handleMergedMessage :: MergedMessage -> HandleMsgM' ()
handleMergedMessage = \case
  MMPresenceAnnouncement ann             -> handlePresenceAnnouncement ann
  MMPresenceAcknowledgement ack          -> handlePresenceAcknowledgement ack
  MMNodeDisconnected disconnected        -> handleNodeDisconnected disconnected

  MMRandomMessage msg                    -> handleRandomMessage msg
  MMRandomMessageAcknowledgement ack     -> handleRandomMessageAck ack

  MMSharePeerStateRequest req            -> handlePeerStateReq req
  MMSharePeerStateResponse resp          -> handlePeerStateResp resp

  MMTick tick                            -> handleTick tick
  MMSynchronizePeersTick tick            -> handleSynchronizePeersTick tick
  MMGracePeriodNotification notification -> handleGracePeriodNotification notification

registerCommunicationPorts
  :: MonadState WorkerState m
  => NodeId
  -> CommunicationPorts
  -> m ()
registerCommunicationPorts nid ports =
  wsPeers . ix nid . psCommunicationPorts .= Just ports

unregisterCommunicationPorts :: MonadState WorkerState m => NodeId -> m ()
unregisterCommunicationPorts nid =
  wsPeers . ix nid . psCommunicationPorts .= Nothing

updateCommunicationMode :: MonadState WorkerState m => CommunicationMode -> m ()
updateCommunicationMode newMode =
  wsCommunicationMode %= max newMode

handlePresenceAnnouncement :: PresenceAnnouncement -> HandleMsgM' ()
handlePresenceAnnouncement (PresenceAnnouncement pid ackSink commPorts) = do
  registerCommunicationPorts (processNodeId pid) commPorts
  we <- ask
  PresenceAcknowledgement (we ^. weWorkerPid) (we ^. weWorkerCommunicationPorts) `sendTo` ackSink
  updateCommunicationMode SendingMessages

handlePresenceAcknowledgement
  :: MonadState WorkerState m
  => PresenceAcknowledgement
  -> m ()
handlePresenceAcknowledgement (PresenceAcknowledgement pid commPorts) = do
  registerCommunicationPorts (processNodeId pid) commPorts
  updateCommunicationMode SendingMessages

handleNodeDisconnected
  :: MonadState WorkerState m
  => NodeDisconnected
  -> m ()
handleNodeDisconnected (NodeDisconnected nid) = unregisterCommunicationPorts nid

-- | Determine message order among other messages depending on sender and message
-- number in the sequence.
messageOrder :: Int -> MsgId -> HandleMsgM' Int
messageOrder senderOrderAmongPeers incomingMessageId = do
  totalPeers <- view weTotalPeers
  pure $ totalPeers * unMsgId incomingMessageId + senderOrderAmongPeers

handleRandomMessage :: RandomMessage -> HandleMsgM' ()
handleRandomMessage RandomMessage{_rmRandomPayload, _rmSender, _rmMsgId, _rmResponseSink} = do
  peerState <- use $ wsPeers . at senderNode
  case peerState of
    Nothing ->
      error $ "Got random message from process not listed among peers: " ++ show senderNode
    Just ps -> do
      msgOrder <- messageOrder (ps ^. psOrderAmongPeers) _rmMsgId
      case acknowledgeIncomingMessage _rmRandomPayload _rmMsgId msgOrder $ ps ^. psCommunicationState of
        -- Don't receive message if a previous one was not received - we won't be
        -- able to bridge the gaps later. Instead, ask our peers for messages from
        -- this process. Since that process sends message we're not expecting, it
        -- must have received acknowledgement from some other process. We'll ask
        -- that other process to share the state instead.
        Nothing                        -> pure ()
        Just updatedCommunicationState -> do
          let ps' = ps & psCommunicationState .~ updatedCommunicationState
          wsPeers . ix senderNode .= ps'
          RandomMessageAcknowledgement _rmMsgId `sendTo` _rmResponseSink
  where
    senderNode = processNodeId _rmSender

handleRandomMessageAck :: RandomMessageAcknowledgement -> HandleMsgM' ()
handleRandomMessageAck (RandomMessageAcknowledgement acknowledgedMsgId) = do
  WorkerState{_wsUnacknowledgedRandomNum, _wsLocalCommState} <- get
  thisProcessOrder <- view weOrderAmongPeers
  msgOrder <- messageOrder thisProcessOrder acknowledgedMsgId
  case acknowledgeIncomingMessage _wsUnacknowledgedRandomNum acknowledgedMsgId msgOrder _wsLocalCommState of
    Nothing               -> pure ()
    Just updatedCommState -> do
      prepareForSendingNextRandomMessage updatedCommState

handleTick :: Tick -> HandleMsgM' ()
handleTick _ = do
  WorkerState{_wsPeers, _wsCommunicationMode} <- get
  case _wsCommunicationMode of
    AwaitingPeers   -> pure ()
    SendingMessages -> do
      -- Keep sending the same message until we gen an acknowledgement.
      currentMessage <- currentRandomMessage
      for_ _wsPeers $ \PeerState{_psCommunicationPorts} ->
        case _psCommunicationPorts of
          Nothing                                  -> pure ()
          Just CommunicationPorts{_cpMessagesSink} ->
            currentMessage `sendTo` _cpMessagesSink
    Waiting -> pure ()
    Done    -> pure ()

handleSynchronizePeersTick :: SynchronizePeersTick -> HandleMsgM' ()
handleSynchronizePeersTick _ = do
  WorkerState{_wsPeers, _wsCommunicationMode} <- get
  case _wsCommunicationMode of
    AwaitingPeers   -> pure ()
    SendingMessages -> do
      let latestMsgId :: Maybe MsgId
          latestMsgId = fmap getMax
                      $ getOption
                      $ foldMap (Option . Just . Max . _csNextExpectedMsgId . _psCommunicationState)
                      $ _wsPeers
      case latestMsgId of
        -- No peers available, don't do anything.
        Nothing       -> pure ()
        Just latestId -> do
          let peersToUpdate :: Set NodeId
              peersToUpdate = M.keysSet
                            $ M.filter ((< latestId) . _csNextExpectedMsgId . _psCommunicationState) _wsPeers
          responseSink <- view $ weWorkerPrivatePorts . ppWorkerSharePeerStateRespSink
          for_ (M.assocs _wsPeers) $ \(nodeToCommunicateWith, ps) ->
            case ps ^. psCommunicationPorts  of
              -- Peer not available.
              Nothing    -> pure ()
              Just ports ->
                for_ peersToUpdate $ \peerNode ->
                  unless (peerNode == nodeToCommunicateWith) $ do
                    let req = SharePeerStateReq
                          { _spsreqRequestedPeer = peerNode
                          , _spsreqResponseSink     = responseSink
                          }
                    req `sendTo` (ports ^. cpSharePeerStateSink)
    Waiting -> pure ()
    Done    -> pure ()


handlePeerStateReq :: SharePeerStateReq -> HandleMsgM' ()
handlePeerStateReq SharePeerStateReq{_spsreqRequestedPeer, _spsreqResponseSink} = do
  peer <- use $ wsPeers . at _spsreqRequestedPeer
  case peer of
    Nothing ->
      error $ "Requested communication state of peer than is not listed among peers: " ++ show _spsreqRequestedPeer
    Just peer -> do
      let response = SharePeerStateResp
            { _spsrespRequestedPeer      = _spsreqRequestedPeer
            , _spsrespCommunicationState = peer ^. psCommunicationState
            }
      response `sendTo` _spsreqResponseSink

handlePeerStateResp :: SharePeerStateResp -> HandleMsgM' ()
handlePeerStateResp SharePeerStateResp{_spsrespRequestedPeer, _spsrespCommunicationState} =
  wsPeers . ix _spsrespRequestedPeer . psCommunicationState %= pickLatestCommunicationState _spsrespCommunicationState

handleGracePeriodNotification :: GracePeriodNotification -> HandleMsgM' ()
handleGracePeriodNotification notification = updateCommunicationMode $
  case notification of
    GracePeriodStarted -> Waiting
    GracePeriodEnded   -> Done

workerRemoteTable :: RemoteTable
workerRemoteTable = initRemoteTable
