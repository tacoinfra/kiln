{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Verification where

import Control.Lens (Prism')
import Control.Lens.TH (makePrisms)
import Data.Either.Validation
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Text.URI as Uri

import Common.App
import Common.Schema
import Tezos.Types

data ForkInfo = ForkInfo
  { _forkInfo_forkStatus :: Either ForkStatus ()
  , _forkInfo_time :: UTCTime
  , _forkInfo_hash :: BlockHash
  } deriving (Show)

data ForkStatus
  = ForkStatus_TooNew
  | ForkStatus_TooOld
  | ForkStatus_Forked
  | ForkStatus_BadNode CacheError
  deriving (Show)
makePrisms ''ForkStatus

instance AsCacheError ForkStatus where
  asCacheError = _ForkStatus_BadNode

class AsForkStatus e where
  asForkStatus :: Prism' e ForkStatus

instance AsForkStatus ForkStatus where
  asForkStatus = id

showForkStatus :: ForkStatus -> Text
showForkStatus = T.pack . \case
  -- ForkStatus_Good -> "good"
  ForkStatus_TooNew -> "new"
  ForkStatus_TooOld -> "block not in chain"
  ForkStatus_Forked -> "forked"
  ForkStatus_BadNode _ -> "no response from node"

onBadForkState :: (ForkInfo -> a) -> ForkInfo -> Validation a ()
onBadForkState k fi = case _forkInfo_forkStatus fi of
  Left ForkStatus_TooOld -> Failure $ k fi {_forkInfo_forkStatus = Left ForkStatus_TooOld}
  Left ForkStatus_Forked -> Failure $ k fi {_forkInfo_forkStatus = Left ForkStatus_Forked}
  _ -> Success ()

validateForkyBlocks :: ([ForkInfo] -> f ()) -> [ForkInfo] -> f ()
validateForkyBlocks f xs = case traverse (onBadForkState pure) xs of
  Success _ -> f []
  Failure bad -> f bad
