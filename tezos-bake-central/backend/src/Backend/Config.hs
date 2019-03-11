{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Backend.Config where

import Control.Lens (Lens', view)
import Control.Monad.Reader (MonadReader, asks)
import Data.Either (fromRight)
import Network.Mail.Mime (Address)
import Text.URI (URI)
import qualified Text.URI as Uri
import qualified Text.URI.QQ as Uri

import Common.URI (Port)
import ExtraPrelude

data AppConfig = AppConfig
  { _appConfig_emailFromAddress :: Address
  , _appConfig_kilnNodePort :: Port
  }

class HasAppConfig a where
  getAppConfig :: Lens' a AppConfig

instance HasAppConfig AppConfig where
  getAppConfig = id

askAppConfig :: (HasAppConfig a, MonadReader a m) => m AppConfig
askAppConfig = asks $ view getAppConfig

kilnNodeURI :: AppConfig -> URI
kilnNodeURI appConfig = fromRight [Uri.uri|http://127.0.0.1:8732|] $
  Uri.mkURI ("http://127.0.0.1:" <> tshow (_appConfig_kilnNodePort appConfig))
