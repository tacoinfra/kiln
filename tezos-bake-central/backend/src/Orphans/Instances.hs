{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Orphans.Instances where

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad.Catch
import Control.Monad.Fail
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans (lift)
import Database.PostgreSQL.Simple
import Rhyolite.Backend.DB.Serializable

instance MonadFail Serializable where
  fail = Control.Monad.Fail.fail

instance MonadIO Serializable where
  liftIO = unsafeMkSerializable . lift . lift

instance MonadCatch Serializable where
  catch (unSerializable -> action) handler = unsafeMkSerializable $ catch action (unSerializable . handler)

instance MonadMask Serializable where
  mask f = unsafeMkSerializable $ mask $ \u -> unSerializable $ f (q u)
    where
      q :: (ReaderT Connection (LoggingT IO) a -> ReaderT Connection (LoggingT IO) a)
        -> Serializable a -> Serializable a
      q u (unSerializable -> b) = unsafeMkSerializable (u b)

  uninterruptibleMask f = unsafeMkSerializable $ uninterruptibleMask $ \u -> unSerializable $ f (q u)
    where
      q :: (ReaderT Connection (LoggingT IO) a -> ReaderT Connection (LoggingT IO) a)
        -> Serializable a -> Serializable a
      q u (unSerializable -> b) = unsafeMkSerializable (u b)

  generalBracket acquire release use = unsafeMkSerializable $
    generalBracket
      (unSerializable acquire)
      (\resource exitCase -> unSerializable (release resource exitCase))
      (\resource -> unSerializable (use resource))
