----------------------------------------------------------------------------
-- |
-- Module      :  Control.Distributed.Process.Ext
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  30 November 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE ImplicitParams #-}

module Control.Distributed.Process.Ext
  ( module Control.Distributed.Process
  , sayDebug
  , putStrLnDebug
  , hPutStrLnDebug
  , DebugLevel(..)
  , readDebugLevel
  ) where

import Control.Distributed.Process
import Control.Monad.IO.Class
import System.IO

data DebugLevel = On | Off
  deriving (Show, Eq, Ord, Enum, Bounded)

readDebugLevel :: String -> Either String DebugLevel
readDebugLevel "on"  = Right On
readDebugLevel "off" = Right Off
readDebugLevel s     = Left $ "Invalid debug level: " ++ s

putStrLnDebug :: (MonadIO m, ?debugLevel :: DebugLevel) => String -> m ()
putStrLnDebug msg = case ?debugLevel of
  On  -> liftIO $ putStrLn msg
  Off -> pure ()

hPutStrLnDebug :: (MonadIO m, ?debugLevel :: DebugLevel) => Handle -> String -> m ()
hPutStrLnDebug h msg = case ?debugLevel of
  On  -> liftIO $ hPutStrLn h msg
  Off -> pure ()

sayDebug :: (?debugLevel :: DebugLevel) => String -> Process ()
sayDebug msg = case ?debugLevel of
  On  -> say msg
  Off -> pure ()
