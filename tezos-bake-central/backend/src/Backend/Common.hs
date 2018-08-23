{-# LANGUAGE OverloadedStrings #-}

module Backend.Common where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Monad (forever, (<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Fixed (Fixed (..))
import Data.Functor (void)
import Data.Ratio (denominator, numerator)
import Data.Semigroup
import Data.Time.Clock (NominalDiffTime (..))
import Rhyolite.Concurrent (supervise)
import System.Timeout (timeout)

import Common (tshow)

nominalDiffTimeToMicroseconds :: NominalDiffTime -> Integer
nominalDiffTimeToMicroseconds n = numerator ratio * (microsecondsInSecond `div` denominator ratio)
  where
    microsecondsInSecond = 10^6
    ratio = toRational n

workerWithDelay :: MonadIO m => IO NominalDiffTime -> (NominalDiffTime -> IO ()) -> m (IO ())
workerWithDelay getDelay f = worker' $ do
  delay <- getDelay
  f delay
  threadDelay' delay

worker' :: MonadIO m => IO () -> m (IO ())
worker' f = return . cancel <=< liftIO $ async $ supervise $ void $ forever f

threadDelay' :: MonadIO m => NominalDiffTime -> m ()
threadDelay' delay = liftIO $ threadDelay (fromIntegral $ nominalDiffTimeToMicroseconds delay)

timeout' :: MonadIO m => NominalDiffTime -> IO a -> m (Maybe a)
timeout' timeLimit f = liftIO $ timeout (fromIntegral $ nominalDiffTimeToMicroseconds timeLimit) f
