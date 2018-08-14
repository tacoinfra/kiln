{-# LANGUAGE OverloadedStrings #-}
module Backend.Common where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forever, (<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Fixed (Fixed (..))
import Data.Functor (void)
import Data.Ratio (denominator, numerator)
import Data.Time.Clock (NominalDiffTime (..))
import Rhyolite.Concurrent (supervise)

import Common (tshow)
import Data.Semigroup
import Say

nominalDiffTimeToMicroseconds :: NominalDiffTime -> Integer
nominalDiffTimeToMicroseconds n = numerator ratio * (microsecondsInSecond `div` denominator ratio)
  where
    microsecondsInSecond = 10^6
    ratio = toRational n

worker' :: MonadIO m => IO NominalDiffTime -> (NominalDiffTime -> IO ()) -> m (IO ())
worker' getDelay f =
  return . killThread <=< liftIO $ forkIO $ supervise $ void $ forever $ do
    delay <- getDelay
    f delay
    threadDelay (fromIntegral $ nominalDiffTimeToMicroseconds delay)
