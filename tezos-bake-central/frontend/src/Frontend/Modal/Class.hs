{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Modal.Class where

import Control.Monad.Reader (MonadReader (ask), ReaderT (..))
import Control.Monad.Trans (MonadTrans (lift))
import Reflex (Event, Reflex, EventWriterT)

class HasModal t m where
  type ModalM m :: * -> *
  tellModal :: Event t (Event t () -> ModalM m (Event t ())) -> m ()

  default tellModal :: (MonadTrans f, m ~ f m', HasModal t m', Monad m', ModalM (f m') ~ ModalM m') => Event t (Event t () -> ModalM m (Event t ())) -> m ()
  tellModal = lift . tellModal

instance (Monad m, Reflex t, HasModal t m) => HasModal t (EventWriterT t w m) where
  type ModalM (EventWriterT t w m) = ModalM m

instance (Monad m, Reflex t, HasModal t m) => HasModal t (ReaderT r m) where
  type ModalM (ReaderT r m) = ReaderT r (ModalM m) -- Transform the modal's monad
  tellModal ev = do
    r <- ask
    lift $ tellModal $ (fmap . fmap) (flip runReaderT r) ev
