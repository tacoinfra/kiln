{-# LANGUAGE OverloadedStrings #-}
module Backend.Supervisor where
import Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Exception.Safe (Handler (..), catch, catches, finally, throwIO)
import Control.Monad (join, unless, void, when, (<=<))
import Say

withTermination :: ((IO a -> IO ()) -> IO b) -> IO b
withTermination k = do
    finalizers <- newTVarIO (return ())
    let addFinalizer f = atomically $ modifyTVar finalizers (f *>)
    finally (k addFinalizer) $ do
      say "TERMINATING"
      join (readTVarIO finalizers)
      say "TERMINATED .. BYE"
