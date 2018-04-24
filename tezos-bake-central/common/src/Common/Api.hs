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
import Common.Schema ()
import Data.Text (Text)
import Focus.Schema ()

instance (Request (PublicRequest Bake), Request (PrivateRequest Bake)) => HasRequest Bake where
  data PublicRequest Bake a where
    PublicRequest_AddClient
      :: Text -- address of client to subscribe to
      -> PublicRequest Bake () -- TODO: perhaps give an Id Client
    PublicRequest_RemoveClient
      :: Text -- address of client to unsubscribe from
      -> PublicRequest Bake ()
  data PrivateRequest Bake a where
    PrivateRequest_NoOp :: PrivateRequest Bake ()

#ifdef USE_TEMPLATE_HASKELL
makeRequestForDataInstance ''PublicRequest ''Bake
makeRequestForDataInstance ''PrivateRequest ''Bake
#else
#include "Api.splices.hs"
#endif