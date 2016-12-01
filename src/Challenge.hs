----------------------------------------------------------------------------
-- |
-- Module      :  Challenge
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 23 November 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Challenge (main) where

import Control.Exception
import Control.Monad.State
import qualified Data.ByteString.Char8 as C8
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Options.Applicative hiding ((<**>))
import System.IO

import Control.Distributed.Process.Ext
import Network.Transport (EndPointAddress(..))

import Challenge.Node
import Challenge.Types
import Challenge.Worker

data NodeConfig = NodeConfig
  { ncfgWorker     :: WorkerConfig
  , ncfgEndPoint   :: EndPointConfig
  , ncfgDebugLevel :: DebugLevel
  } deriving (Show, Eq, Ord)

endPointOpts :: Parser EndPointConfig
endPointOpts = EndPointConfig
  <$> strOption
        (long "host" <>
         help "Host name" <>
         metavar "HOST" <>
         value defaultHostName)
  <*> strOption
        (long "port" <>
         help "Length of interval for sending messages in seconds" <>
         metavar "INTEGER" <>
         value defaultPort)

workerOpts :: Parser (IO WorkerConfig)
workerOpts = mkWorkerConfig
  <$> option auto
        (long "with-seed" <>
         help "Seed for random number generator" <>
         metavar "INTEGER")
  <*> option (fromSeconds <$> auto)
        (long "send-for" <>
         help "Length of interval for sending messages in seconds" <>
         metavar "SECONDS")
  <*> option (fromSeconds <$> auto)
        (long "wait-for" <>
         help "Length of interval for grace period in seconds" <>
         metavar "SECONDS")
  <*> strOption
        (long "nodes" <>
         help "File with other nodes" <>
         metavar "FILE")
  <*> (fromSeconds <$>
       option auto
         (long "tick-interval" <>
          help "File with other nodes" <>
          value 1 <>
          metavar "SECONDS"))
  <*> (fromSeconds <$>
       option auto
         (long "peer-synchronization-interval" <>
          help "Synchronize state with other peers this often" <>
          value 1 <>
          metavar "SECONDS"))
  where
    mkWorkerConfig seed sendInterval waitInterval slaveNodesFile tickInterval synchronizePeersInterval = do
      nodes <- readNodes <$> C8.readFile slaveNodesFile
      pure WorkerConfig
        { wcfgRandomSeed                      = seed
        , wcfgSendInterval                    = sendInterval
        , wcfgWaitInterval                    = waitInterval
        , wcfgPeers                           = nodes
        , wcfgTickInterval                    = tickInterval
        , wcfgPeerSynchronizationTickInterval = synchronizePeersInterval
        }

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<**>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<**>) f x = (\f' x' -> f' <*> x') <$> f <*> x

nodeOpts :: Parser (IO NodeConfig)
nodeOpts = NodeConfig
  <$$> workerOpts
  <**> (pure <$> endPointOpts)
  <**> (pure <$>
        option (eitherReader readDebugLevel)
          ( long "debug-messages" <>
            metavar "{on, off}" <>
            value Off <>
            help "whether to enable debug logging"))



nodeInfo :: ParserInfo (IO NodeConfig)
nodeInfo = info
  (helper <*> nodeOpts)
  (fullDesc <> header "Run peer node that will send messages to other nodes")

data Mode = Node NodeConfig
  deriving (Show, Eq, Ord)

options :: Parser (IO Mode)
options =
  fmap Node <$> subparser (command "node" nodeInfo <> metavar "node")

progInfo :: ParserInfo (IO Mode)
progInfo = info
  (helper <*> options)
  (fullDesc <> header "CH/OTP test task")

readNodes :: C8.ByteString -> Set NodeId
readNodes = S.fromList . map (NodeId . EndPointAddress) . C8.lines

main :: IO ()
main = do
  config <- join $ execParser progInfo
  case config of
    Node NodeConfig{ncfgEndPoint, ncfgWorker, ncfgDebugLevel} ->
      handle
        (\(e :: SomeException) -> do
          hFlush stdout
          hFlush stderr
          hPutStrLn stderr $ "Got exception: " ++ show e) $ do
        -- putStrLn $ "ncfgDebugLevel = " ++ show ncfgDebugLevel
        let ?debugLevel = ncfgDebugLevel
        hPutStrLnDebug stderr $ "Config = " ++ show config
        runNode workerRemoteTable ncfgEndPoint $ worker ncfgWorker
        hPutStrLnDebug stderr "Done running node"

