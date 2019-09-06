{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Common where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, withAsync, waitCatch)
import Control.Exception (SomeException, try)
import Control.Monad (forever, (<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebug)
import Data.Functor (void)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Rhyolite.Concurrent (supervise)
import System.Exit (ExitCode)
import qualified System.Process as Process
import System.Timeout (timeout)

import Common (nominalDiffTimeToMicroseconds)
import ExtraPrelude

workerWithDelay :: MonadIO m => IO NominalDiffTime -> (NominalDiffTime -> IO ()) -> m (IO ())
workerWithDelay getDelay f = worker' $ do
  delay <- getDelay
  f delay
  threadDelay' delay

worker' :: MonadIO m => IO () -> m (IO ())
worker' f = return . cancel <=< liftIO $ async $ supervise $ void $ forever f

oneShot :: MonadIO m => IO () -> m (IO ())
oneShot f = return . cancel <=< liftIO $ async $ superviseOneShot $ void $ f

superviseOneShot :: IO () -> IO ()
superviseOneShot a = go
 where
   go = do
     withAsync a $ \child -> do
       waitCatch child >>= \case
         Right () -> return ()
         Left (err :: SomeException) -> do
            printResult :: Either SomeException () <-
              try $ putStrLn $ "supervise: child terminated with " <> show err <> "; restarting"
            threadDelay 1000000
            when (isLeft printResult) $ putStrLn "supervise: note: an exception was encountered when printing the previous result"
            go

-- Like 'workerWithDelay' but without a supervising thread. Use this when you don't
-- want your thread to be restarted without you controlling how that happens.
unsupervisedWorkerWithDelay :: MonadIO m => NominalDiffTime -> IO () -> m (IO ())
unsupervisedWorkerWithDelay delay f =
  return . cancel <=< liftIO $ async $ forever $ f *> threadDelay' delay

threadDelay' :: MonadIO m => NominalDiffTime -> m ()
threadDelay' delay = liftIO $ threadDelay (fromIntegral $ nominalDiffTimeToMicroseconds delay)

timeout' :: MonadIO m => NominalDiffTime -> IO a -> m (Maybe a)
timeout' timeLimit f = liftIO $ timeout (fromIntegral $ nominalDiffTimeToMicroseconds timeLimit) f

readCreateProcessWithExitCodeWithLogging
  :: (MonadIO m, MonadLogger m)
  => Process.CreateProcess -> Text -> m (ExitCode, Text, Text)
readCreateProcessWithExitCodeWithLogging cp stdin = do
  $(logDebug) $ "readProcessWithExitCode: " <> tshow cp <> " with STDIN: " <> stdin
  (ec, stdOut, stdErr) <- liftIO $ Process.readCreateProcessWithExitCode cp $ T.unpack stdin
  pure (ec, T.pack stdOut, T.pack stdErr)
