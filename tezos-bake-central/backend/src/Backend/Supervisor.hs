{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Supervisor where

import Control.Monad.Logger (LoggingT, logInfo)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Exception.Safe (finally)
import Control.Monad (join)

withTermination :: ((IO a -> IO ()) -> IO b) -> LoggingT IO b
withTermination k = do
  finalizers <- liftIO $ newTVarIO (return ())
  let addFinalizer f = atomically $ modifyTVar finalizers (f *>)
  finally (liftIO $ k addFinalizer) $ do
    $(logInfo) "TERMINATING"
    liftIO $ join (readTVarIO finalizers)
    $(logInfo) "TERMINATED .. BYE"
