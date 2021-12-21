{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Backend.STM where

import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, retry, writeTVar)
import Control.Lens (Lens')
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), ask)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Time (UTCTime, getCurrentTime)

class Monad m => MonadSTM m where
  liftSTM :: STM a -> m a
  default liftSTM :: forall t a m'. (m ~ t m', MonadTrans t, MonadSTM m') => STM a -> m a
  liftSTM = lift . liftSTM

instance MonadSTM STM where
  liftSTM = id

instance MonadSTM IO where
  liftSTM = atomically

instance MonadSTM m => MonadSTM (ReaderT r m)
instance MonadSTM m => MonadSTM (ExceptT e m)
instance MonadSTM m => MonadSTM (MaybeT m)

class HasTimestamp r where
  timestamp :: Lens' r UTCTime

instance HasTimestamp UTCTime where
  timestamp = id

type STMWithTime r m = (MonadSTM m, MonadReader r m, HasTimestamp r)

atomicallyWith :: forall s m a. (MonadReader s m, MonadIO m) => ReaderT s STM a -> m a
atomicallyWith f = do
  a <- ask
  liftIO $ atomically $ runReaderT f a

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

