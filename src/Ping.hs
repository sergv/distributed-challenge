{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Ping (main) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import Text.Printf

import DistribUtils

data Msg =
    Ping ProcessId
  | Pong ProcessId
  deriving (Show, Eq, Ord, Generic, Typeable)

instance Binary Msg

pingServer :: Process ()
pingServer = do
  Ping from <- expect
  mypid     <- getSelfPid
  say $ printf "Ping of %s from %s\n" (show mypid) (show from)
  send from $ Pong mypid
  -- pingServer

-- Derive infrastructure to allow 'pingServer' to be called remotely.
remotable ['pingServer]

master :: Process ()
master = do
  node <- getSelfNode
  say $ printf "Spawning ping server on %s\n" (show node)
  pid   <-spawn node $(mkStaticClosure 'pingServer)
  mypid <- getSelfPid
  say $ printf "Sending ping to %s\n" (show pid)
  send pid $ Ping mypid
  (response :: Msg) <- expect
  say $ printf "Got response %s\n" (show response)
  terminate

main :: IO ()
main = do
  putStrLn "Hello, Distributed Haskell!"
  distribMain (const master) Ping.__remoteTable
