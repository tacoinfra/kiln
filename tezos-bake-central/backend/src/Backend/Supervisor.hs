{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Supervisor where

import Control.Monad.Logger (LoggingT, logInfo)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans (lift)
import UnliftIO.STM (atomically, modifyTVar, newTVarIO, readTVarIO)
import UnliftIO.Exception (finally)
import Control.Monad (join)

withTermination :: MonadUnliftIO m => ((m a -> m ()) -> m b) -> LoggingT m b
withTermination k = do
  finalizers <- liftIO $ newTVarIO (return ())
  let addFinalizer f = atomically $ modifyTVar finalizers (f *>)
  finally (lift $ k addFinalizer) $ do
    $(logInfo) "TERMINATING"
    lift $ join (readTVarIO finalizers)
    $(logInfo) "TERMINATED .. BYE"
