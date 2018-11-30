{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Backend.STM where

import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, retry, writeTVar)
import Control.Lens (Lens')
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans (MonadTrans (lift))
import Data.Time (UTCTime, getCurrentTime)

class Monad m => MonadSTM m where
  liftSTM :: STM a -> m a
  default liftSTM :: forall t a m'. (m ~ t m', MonadTrans t, MonadSTM m') => STM a -> m a
  liftSTM = lift . liftSTM

instance MonadSTM STM where
  liftSTM = id

instance MonadSTM m => MonadSTM (ReaderT r m)
instance MonadSTM m => MonadSTM (ExceptT e m)

class HasTimestamp r where
  timestamp :: Lens' r UTCTime

instance HasTimestamp UTCTime where
  timestamp = id

atomicallyWithTime :: MonadIO m => ReaderT UTCTime STM a -> m a
atomicallyWithTime act = liftIO $ do
  now <- getCurrentTime
  atomically $ runReaderT act now

readTVar' :: MonadSTM m => TVar a -> m a
readTVar' = liftSTM . readTVar

writeTVar' :: MonadSTM m => TVar a -> a -> m ()
writeTVar' t = liftSTM . writeTVar t

newTVar' :: MonadSTM m => a -> m (TVar a)
newTVar' = liftSTM . newTVar

retry' :: MonadSTM m => m a
retry' = liftSTM retry

modifyTVar_' :: MonadSTM m => TVar a -> (a -> m a) -> m ()
modifyTVar_' t f = readTVar' t >>= f >>= writeTVar' t

