{-# LANGUAGE TemplateHaskell #-}

module Backend.Env
  ( KilnEnv (..)

    -- * Functions to retrieve the environment components.
  , askAppConfig
  , askChainId
  , askLogger
  , askPool
  , askHttpMgr

    -- * Functions to run monadic actions.
  , runLogger
  , runTransaction
  ) where


import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Logger (LoggingT, MonadLoggerIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
import Data.Pool (Pool)
import Database.Groundhog.Postgresql (Postgresql(..))
import qualified Network.HTTP.Client as Http
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.DB.Serializable (Serializable)
import Rhyolite.Backend.Logging (LoggingEnv, runLoggingEnv)

import Backend.Config (AppConfig (..), HasAppConfig(..), askAppConfig)
import Backend.NodeRPC (NodeDataSource(..), HasNodeDataSource(..), nodeDataSource_httpMgr, nodeDataSource_logger, nodeDataSource_pool)
import Tezos.Types (ChainId)

data KilnEnv = KilnEnv
  { _kilnEnv_appConfig :: AppConfig
  , _kilnEnv_nodeDataSource :: NodeDataSource
  }

-- | Get @LoggingEnv@ from @KilnEnv@.
askLogger
  :: ( MonadReader e m
     , HasNodeDataSource e
     )
  => m LoggingEnv
askLogger = asks (\e -> e ^. nodeDataSource . nodeDataSource_logger)

-- | Get @Pool Postgresql@ from @KilnEnv@.
askPool
  :: ( MonadReader e m
     , HasNodeDataSource e
     )
  => m (Pool Postgresql)
askPool = asks (\e -> e ^. nodeDataSource . nodeDataSource_pool)

-- | Get @Http.Manager@ from @KilnEnv@.
askHttpMgr
  :: ( MonadReader e m
     , HasNodeDataSource e
     )
  => m Http.Manager
askHttpMgr = asks (\e -> e ^. nodeDataSource . nodeDataSource_httpMgr)

-- | Get @ChainId@ from @KilnEnv@.
askChainId
  :: ( MonadReader e m
     , HasAppConfig e
     )
  => m ChainId
askChainId = asks (\e -> e ^. getAppConfig & _appConfig_chainId)

-- | Run database transaction using the @Pool Posgresql@ from
-- the environment.
runTransaction
  :: ( MonadLoggerIO m
     , MonadReader e m
     , HasAppConfig e
     , HasNodeDataSource e
     )
  => Serializable a
  -> m a
runTransaction tx = do
  db <- askPool
  runDb (Identity db) tx

-- | Run @LoggingT@ monad using the @LoggingEnv@ from
-- the environment.
runLogger
  :: ( MonadReader e m
     , HasNodeDataSource e
     )
  => LoggingT m a
  -> m a
runLogger action = do
  logger <- askLogger
  runLoggingEnv logger action

makeLenses 'KilnEnv

instance HasAppConfig KilnEnv where
  getAppConfig = kilnEnv_appConfig

instance HasNodeDataSource KilnEnv where
  nodeDataSource = kilnEnv_nodeDataSource
