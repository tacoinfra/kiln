{-# LANGUAGE FlexibleInstances #-}

module Orphans.Instances where

import Control.Monad.Fail
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import Control.Monad.Logger

import Prelude hiding (fail)

instance MonadFail m => MonadFail (DbPersist Postgresql m) where
  fail = fail
