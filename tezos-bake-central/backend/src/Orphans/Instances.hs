{-# LANGUAGE FlexibleInstances #-}

module Orphans.Instances where

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad.Fail
import Database.Groundhog.Core
import Database.Groundhog.Postgresql

import Prelude hiding (fail)

instance MonadFail m => MonadFail (DbPersist Postgresql m) where
  fail = fail
