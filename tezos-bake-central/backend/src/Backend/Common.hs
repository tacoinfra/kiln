
module Backend.Common
  ( module X
  , AppSerializable
  )
  where

import Backend.Common.Baker as X
import Backend.Common.Node as X
import Backend.Common.Worker as X
import Backend.Common.TezosRelease as X

import Backend.Config
import Control.Monad.Reader
import Rhyolite.Backend.DB.Serializable (Serializable)

type AppSerializable a = ReaderT AppConfig Serializable a
