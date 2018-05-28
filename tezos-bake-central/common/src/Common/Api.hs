{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Common.Api where

import Focus.App
import Focus.Request
import Common.App
import Common.Schema (ClientAddress())
import Focus.Schema (Email)

import Data.Fixed
import Data.Text (Text)

instance (Request (PublicRequest Bake), Request (PrivateRequest Bake)) => HasRequest Bake where
  data PublicRequest Bake a where
    PublicRequest_AddClient
      :: ClientAddress -- address of client to subscribe to
      -> PublicRequest Bake () -- TODO: perhaps give an Id Client
    PublicRequest_RemoveClient
      :: ClientAddress -- address of client to unsubscribe from
      -> PublicRequest Bake ()
    PublicRequest_AddNotificatee
      :: Email
      -> PublicRequest Bake ()
    PublicRequest_RemoveNotificatee
      :: Email
      -> PublicRequest Bake ()
    PublicRequest_RenderGraph -- temporary while I write a Reflex backend for Chart
      :: Text
      -> [(Integer,Micro)]
      -> PublicRequest Bake Text
  data PrivateRequest Bake a where
    PrivateRequest_NoOp :: PrivateRequest Bake ()

#ifdef USE_TEMPLATE_HASKELL
makeRequestForDataInstance ''PublicRequest ''Bake
makeRequestForDataInstance ''PrivateRequest ''Bake
#else
#include "Api.splices.hs"
#endif
