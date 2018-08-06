module Backend.Supervisor where
import Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Exception.Safe (Handler (..), catch, catches, finally, throwIO)
import Control.Monad (join, unless, void, when, (<=<))

supervise k = do
    finalizers <- newTVarIO (return ())
    let addFinalizer f = atomically $ modifyTVar finalizers (f *>)
    k addFinalizer `finally` join (readTVarIO finalizers)
