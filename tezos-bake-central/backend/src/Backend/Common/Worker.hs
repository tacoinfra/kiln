{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend.Common.Worker where

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, logDebug)
import Data.Either.Combinators (whenLeft)
import Data.Functor (void)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
-- import Rhyolite.Concurrent (supervise)
import System.Exit (ExitCode)
import qualified UnliftIO.Process as Process
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (SomeException, try)
import UnliftIO.Async (async, cancel, withAsync, waitCatch)
import UnliftIO.Timeout (timeout)

import Common (nominalDiffTimeToMicroseconds)
import ExtraPrelude

workerWithDelay :: (MonadUnliftIO m, MonadUnliftIO w) => String -> m NominalDiffTime -> (NominalDiffTime -> m ()) -> m (w ())
workerWithDelay caller getDelay f = worker' caller $ do
  delay <- getDelay
  f delay
  threadDelay' delay

worker' :: (MonadUnliftIO m, MonadUnliftIO w) => String -> m () -> m (w ())
worker' caller f = return . cancel =<< async (supervise caller $ void $ forever f)

supervise :: (MonadUnliftIO m, Show a) => String -> m a -> m ()
supervise caller a = forever $ withAsync a $ \child -> do
  result <- waitCatch child
  printResult :: Either SomeException () <- try $ liftIO $ putStrLn $ "supervise (called from: " <> caller <> "): child terminated with " <> show result <> "; restarting"
  threadDelay 1000000
  whenLeft printResult $ \_ -> liftIO $
    putStrLn "supervise: note: an exception was encountered when printing the previous result"

-- Like 'workerWithDelay' but without a supervising thread. Use this when you don't
-- want your thread to be restarted without you controlling how that happens.
unsupervisedWorkerWithDelay :: (MonadUnliftIO m, MonadUnliftIO w) => NominalDiffTime -> m () -> m (w ())
unsupervisedWorkerWithDelay delay f =
  return . cancel =<< async (forever $ f *> threadDelay' delay)

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
