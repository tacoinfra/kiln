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

-- Like 'workerWithDelay' but without a supervising thread. Use this when you don't
-- want your thread to be restarted without you controlling how that happens.
unsupervisedWorkerWithDelay :: MonadIO m => NominalDiffTime -> IO () -> m (IO ())
unsupervisedWorkerWithDelay delay f =
  return . cancel <=< liftIO $ async $ forever $ f *> threadDelay' delay

threadDelay' :: MonadIO m => NominalDiffTime -> m ()
threadDelay' delay = liftIO $ threadDelay (fromIntegral $ nominalDiffTimeToMicroseconds delay)

timeout' :: MonadIO m => NominalDiffTime -> IO a -> m (Maybe a)
timeout' timeLimit f = liftIO $ timeout (fromIntegral $ nominalDiffTimeToMicroseconds timeLimit) f
