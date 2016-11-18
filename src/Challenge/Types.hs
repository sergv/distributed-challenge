----------------------------------------------------------------------------
-- |
-- Module      :  Challenge.Types
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

{-# LANGUAGE NamedFieldPuns #-}

module Challenge.Types
  ( EndPointConfig(..)
  , defaultHostName
  , defaultPort
  , makeTransport
  ) where

import Network.Socket (HostName, ServiceName)

import Network.Transport (Transport)
import Network.Transport.TCP (createTransport, defaultTCPParameters)

data EndPointConfig = EndPointConfig
  { ecfgHost :: HostName
  , ecfgPort :: ServiceName
  } deriving (Show, Eq, Ord)

defaultHostName :: HostName
defaultHostName = "127.0.0.1"

defaultPort :: ServiceName
defaultPort = "10501"

makeTransport :: EndPointConfig -> IO Transport
makeTransport EndPointConfig{ecfgHost, ecfgPort} = do
  t <- createTransport ecfgHost ecfgPort defaultTCPParameters
  case t of
    Left e  -> ioError e
    Right x -> pure x
