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
import Common.Schema
import Data.Text (Text)
import Focus.Schema

instance (Request (PublicRequest Bake), Request (PrivateRequest Bake)) => HasRequest Bake where
  data PublicRequest Bake a where
    PublicRequest_Doot :: PublicRequest Bake ()
  data PrivateRequest Bake a where
    PrivateRequest_AddClient :: Client -> PrivateRequest Bake (Either Text (Id Client))
    PrivateRequest_RemoveClient :: Id Client -> PrivateRequest Bake ()

#ifdef USE_TEMPLATE_HASKELL
makeRequestForDataInstance ''PublicRequest ''Bake
makeRequestForDataInstance ''PrivateRequest ''Bake
#else
#include "Api.splices.hs"
#endif