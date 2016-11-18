----------------------------------------------------------------------------
-- |
-- Module      :  Challenge.Worker.Types
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

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DoAndIfThenElse            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Challenge.Worker.Types
  ( RandomMessage(..)
  , rmRandomPayload
  , rmSender
  , rmMsgId
  , rmResponseSink
  , RandomMessageAcknowledgement(..)
  , rmaMsgId
  , PresenceAnnouncement(..)
  , PresenceAcknowledgement(..)
  , NodeDisconnected(..)
  , Tick(..)
  , SynchronizePeersTick(..)
  , GracePeriodNotification(..)
  , SharePeerStateReq(..)
  , spsreqRequestedPeer
  , spsreqResponseSink
  , SharePeerStateResp(..)
  , spsrespRequestedPeer
  , spsrespCommunicationState
  , WorkerEnv(..)
  , weWorkerPid
  , weWorkerCommunicationPorts
  , weWorkerPrivatePorts
  , weTotalPeers
  , weOrderAmongPeers
  , PrivatePorts(..)
  , ppWorkerSharePeerStateRespSink
  , ppWorkerMessageAckSink
  , CommunicationPorts(..)
  , cpMessagesSink
  , cpSharePeerStateSink
  , CommunicationState(..)
  , acknowledgeIncomingMessage
  , pickLatestCommunicationState
  , csNextExpectedMsgId
  , csStateSum
  , PeerState(..)
  , psCommunicationPorts
  , psCommunicationState
  , psOrderAmongPeers
  , CommunicationMode(..)
  , WorkerState(..)
  , wsPeers
  , wsCommunicationMode
  , wsRandomGen
  , wsUnacknowledgedRandomNum
  , wsLocalCommState
  , mkWorkerState
  ) where

import Data.Binary (Binary)
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.Random

import Control.Distributed.Process hiding (bracket)

import Data.MsgId
import Data.RandomNumber

import Lens.Micro.TH

-- | Main message type.
data RandomMessage = RandomMessage
  { _rmRandomPayload :: RandomNumber
  , _rmSender        :: ProcessId
  , _rmMsgId         :: MsgId
  , _rmResponseSink  :: SendPort RandomMessageAcknowledgement
  } deriving (Show, Eq, Ord, Generic, Typeable)

instance Binary RandomMessage

data RandomMessageAcknowledgement = RandomMessageAcknowledgement
  { _rmaMsgId :: MsgId
  } deriving (Show, Eq, Ord, Generic, Typeable)

instance Binary RandomMessageAcknowledgement

-- | Announce that some process is ready to exchange messages.
data PresenceAnnouncement =
  PresenceAnnouncement
    ProcessId
    (SendPort PresenceAcknowledgement)
    CommunicationPorts
  deriving (Show, Eq, Ord, Generic)

instance Binary PresenceAnnouncement

-- | Acknowledge the 'PresenceAnonuncement' by sharing credentials of the receiver.
data PresenceAcknowledgement =
  PresenceAcknowledgement
    ProcessId
    CommunicationPorts
  deriving (Show, Eq, Ord, Generic)

instance Binary PresenceAcknowledgement

-- | Notify process that one of its peers has disconnected.
newtype NodeDisconnected = NodeDisconnected NodeId
  deriving (Show, Eq, Ord, Generic)

instance Binary NodeDisconnected

newtype Tick = Tick MsgId
  deriving (Show, Eq, Ord, Generic)

instance Binary Tick

newtype SynchronizePeersTick = SynchronizePeersTick MsgId
  deriving (Show, Eq, Ord, Generic)

instance Binary SynchronizePeersTick

data GracePeriodNotification =
    -- | Request to cease producing new messages and to synchronize state.
    GracePeriodStarted
    -- | Request to terminate program.
  | GracePeriodEnded
  deriving (Show, Eq, Ord, Generic)

instance Binary GracePeriodNotification

-- | Request fellow node to share it's state of communicating with some other
-- process.
data SharePeerStateReq = SharePeerStateReq
  { _spsreqRequestedPeer :: NodeId
  , _spsreqResponseSink  :: SendPort SharePeerStateResp
  } deriving (Show, Eq, Ord, Generic)

instance Binary SharePeerStateReq

data SharePeerStateResp = SharePeerStateResp
  { _spsrespRequestedPeer      :: NodeId
  , _spsrespCommunicationState :: CommunicationState
  } deriving (Show, Eq, Ord, Generic)

instance Binary SharePeerStateResp

-- | Reader env for requst handlers.
data WorkerEnv = WorkerEnv
  { -- | Pid of the process current worker is running in.
    _weWorkerPid                :: ProcessId
  , _weWorkerCommunicationPorts :: CommunicationPorts
  , _weWorkerPrivatePorts       :: PrivatePorts
  , _weTotalPeers               :: Int
    -- | Order of local process among all of the peers.
  , _weOrderAmongPeers          :: Int
  } deriving (Show, Eq, Ord)

-- | Communication ports that should not generally be visible to other processes.
-- These will be included in requests in order to get relevant responses, but
-- should not be directly used by other processes to send anything to the current
-- process.
data PrivatePorts = PrivatePorts
  { _ppWorkerSharePeerStateRespSink :: SendPort SharePeerStateResp
  , _ppWorkerMessageAckSink         :: SendPort RandomMessageAcknowledgement
  } deriving (Show, Eq, Ord)

-- | Public ports that will be broadcast and can be used to communicate with the
-- given process.
data CommunicationPorts = CommunicationPorts
  { _cpMessagesSink       :: SendPort RandomMessage
  , _cpSharePeerStateSink :: SendPort SharePeerStateReq
  } deriving (Show, Eq, Ord, Generic, Typeable)

instance Binary CommunicationPorts

-- | Progress in communicating with other process. Stores sum of messages
-- sent by the other process.
data CommunicationState = CommunicationState
  { -- | Next message expected to be received from this peer and that should
    -- be acknowledged.
    _csNextExpectedMsgId :: MsgId
    -- | Sum of products of messages sent by this process and message orders
    -- within global ordering of processes.
  , _csStateSum          :: Double
  } deriving (Show, Eq, Ord, Generic, Typeable)

instance Binary CommunicationState

-- | Take incoming random payload and message id, i.e. sequence number, and
-- integrate it it with state sum seen so far.
--
-- Returns Nothing if integration was not successful and Just otherwise.
-- Integration fails if incoming message has different message id than the one
-- we were expecting, i.e. if this message violates continuity of the sequence
-- sequence seen so far. In other words, it fails if message was already sent
-- previously or previous message w.r.t. current one was not received because
-- we won't be able to bridge the gaps later.
acknowledgeIncomingMessage
  :: RandomNumber
  -> MsgId
  -> Int -- ^ Global message order.
  -> CommunicationState
  -> Maybe CommunicationState
acknowledgeIncomingMessage payload msgId msgOrder CommunicationState{_csNextExpectedMsgId, _csStateSum}
  | msgId == _csNextExpectedMsgId
  = Just CommunicationState
      { _csNextExpectedMsgId = incrementMsgId _csNextExpectedMsgId
      , _csStateSum          = _csStateSum + sumIncrement
      }
  | otherwise
  = Nothing
  where
    sumIncrement = fromIntegral msgOrder * unRandomNumber payload

pickLatestCommunicationState :: CommunicationState -> CommunicationState -> CommunicationState
pickLatestCommunicationState s@(CommunicationState msgId x) s'@(CommunicationState msgId' x')
  | msgId > msgId' && x > x' = s
  | otherwise                = s'

-- | State of communication with particular process.
data PeerState = PeerState
  { _psCommunicationPorts :: Maybe CommunicationPorts
  , _psCommunicationState :: CommunicationState
    -- | Positive number unique to the process in question.
  , _psOrderAmongPeers    :: Int
  } deriving (Show, Eq, Ord)

-- | NB order is critical for this enum.
data CommunicationMode =
    AwaitingPeers   -- ^ Don't send any messages, wait until at least one peer appears.
  | SendingMessages -- ^ Send messages to other peers, reply to messages from them.
  | Waiting         -- ^ Grace period. Don't send any messages ourselves, only reply to other's.
  | Done            -- ^ Finish and shut down the node.
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | State for request handlers.
data WorkerState = WorkerState
  { _wsPeers                   :: Map NodeId PeerState
  , _wsCommunicationMode       :: CommunicationMode
  , _wsRandomGen               :: StdGen
  , _wsUnacknowledgedRandomNum :: RandomNumber
  -- | Partial sum for the current process. This is a reconstruction of the state
  -- as seen by at least one of our peers.
  , _wsLocalCommState          :: CommunicationState
  } deriving Show

mkWorkerState
  :: NodeId     -- ^ Node of the process to create state for.
  -> Set NodeId -- ^ Global set of peers.
  -> Int
  -> (WorkerState, Int)
mkWorkerState mynid peers wcfgRandomSeed =
  ( newState
  , maybe
      (error $ "Node " ++ show mynid ++ "is not registered among peers")
      _psOrderAmongPeers
      myPeerState
  )
  where
    peersMap :: Map NodeId PeerState
    peersMap =
      M.fromList $
      zipWith (\order node -> (node, initPeerState order)) [1..] $
      toList peers
      -- -- Remove ourselves since we should not send messages to ourselves.
      -- -- However, we must use full set of peers here, in order to
      -- -- establish global ordering among them.
      -- M.delete mynid $

    (myPeerState, otherPeersMap) = M.updateLookupWithKey (\_ _ -> Nothing) mynid peersMap

    newState = WorkerState
      { _wsPeers                   = otherPeersMap
      , _wsCommunicationMode       = AwaitingPeers
      , _wsRandomGen               = gen
      , _wsUnacknowledgedRandomNum = randomNum
      , _wsLocalCommState          = initCommunicationState
      }

    randomNum :: RandomNumber
    gen       :: StdGen
    (randomNum, gen) = genRandomNumber $ mkStdGen wcfgRandomSeed

    initPeerState :: Int -> PeerState
    initPeerState order = PeerState
      { _psCommunicationPorts  = Nothing
      , _psCommunicationState  = initCommunicationState
      , _psOrderAmongPeers     = order
      }

    initCommunicationState :: CommunicationState
    initCommunicationState = CommunicationState
      { _csNextExpectedMsgId = initMsgId
      , _csStateSum          = 0
      }

makeLenses ''RandomMessage
makeLenses ''RandomMessageAcknowledgement
makeLenses ''SharePeerStateReq
makeLenses ''SharePeerStateResp
makeLenses ''WorkerEnv
makeLenses ''PrivatePorts
makeLenses ''CommunicationPorts
makeLenses ''CommunicationState
makeLenses ''PeerState
makeLenses ''WorkerState

