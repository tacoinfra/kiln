{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Backend.Http where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT), mapReaderT)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Aeson (FromJSON)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Simple as Http

data Request a where
  Request_JSON :: FromJSON a => Http.Request -> Request a

class HasHttp m where
  req :: Request a -> m (Http.Response a)

newtype HttpT m a = HttpT
  { unHttpT :: ReaderT Http.Manager m a
  } deriving (Monad, Functor, Applicative, MonadTrans, MonadIO, MonadThrow, MonadLogger)

instance (Monad m, MonadIO m) => HasHttp (HttpT m) where
  req r' = HttpT $ do
    mgr <- ask
    case r' of
      Request_JSON r -> Http.httpJSON (Http.setRequestManager mgr r)

runHttpT :: Http.Manager -> HttpT m a -> m a
runHttpT httpMgr (HttpT f) = runReaderT f httpMgr

instance MonadReader r m => MonadReader r (HttpT m) where
  ask = lift ask
  local f (HttpT a) = HttpT $ mapReaderT (local f) a
