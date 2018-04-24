{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Backend.Schema where

import Database.Groundhog.Instances ()
import Database.Groundhog.Postgresql ()
import Focus.Backend.Account ()
import Focus.Backend.DB.Groundhog (groundhog, mkFocusPersist)
import Focus.Backend.Schema.TH

import Common.Schema

mkFocusPersist (Just "migrateSchema") [groundhog|
  - entity: Client
|]

fmap concat $ mapM (uncurry makeDefaultKeyIdInt64)
  [
  ]
