
module Backend.Common
  ( module X
  , AppSerializable
  , withDbAndConfig
  )
  where

import Control.Monad.Logger (MonadLoggerIO)
import Data.Functor.Identity (Identity(..))
import Data.Pool (Pool)
import Database.Groundhog.Postgresql (Postgresql(..))

import Backend.Common.Baker as X
import Backend.Common.Ledger as X
import Backend.Common.Node as X
import Backend.Common.Worker as X
import Backend.Common.TezosRelease as X

import Backend.Config
import Control.Monad.Reader
import Rhyolite.Backend.DB (RunDb(..))
import Rhyolite.Backend.DB.Serializable (Serializable)

type AppSerializable a = ReaderT AppConfig Serializable a

withDbAndConfig :: MonadLoggerIO m => Pool Postgresql -> AppConfig -> AppSerializable a -> m a
withDbAndConfig db appConfig = runDb (Identity db) . flip runReaderT appConfig
