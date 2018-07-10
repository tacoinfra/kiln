{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Verification where

import Data.Either.Validation
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import Common.Schema
import Common.TaggedHash (BlockHash, toBase58Text)

data ForkInfoF e = ForkInfo
  { _forkInfo_node :: Node
  , _forkInfo_forkStatus :: ForkStatusF e
  , _forkInfo_time :: UTCTime
  , _forkInfo_hash :: BlockHash
  } deriving (Eq, Ord, Show)

data ForkStatusF e
  = ForkStatus_Good
  | ForkStatus_TooNew
  | ForkStatus_TooOld
  | ForkStatus_Forked
  | ForkStatus_BadNode e
  deriving (Eq, Ord, Show)

showForkStatus :: ForkStatusF e -> Text
showForkStatus = T.pack . \case
  ForkStatus_Good -> "good"
  ForkStatus_TooNew -> "new"
  ForkStatus_TooOld -> "block not in chain"
  ForkStatus_Forked -> "forked"
  ForkStatus_BadNode _ -> "no response from node"

onBadForkState :: (ForkInfoF () -> a) -> ForkInfoF e -> Validation a ()
onBadForkState k fi = case _forkInfo_forkStatus fi of
  ForkStatus_TooOld -> Failure $ k fi {_forkInfo_forkStatus = ForkStatus_TooOld}
  ForkStatus_Forked -> Failure $ k fi {_forkInfo_forkStatus = ForkStatus_Forked}
  _ -> Success ()

showBadFork :: ForkInfoF e -> Error
showBadFork (ForkInfo node status bakedTime bakedHash) = Error bakedTime $ T.concat
          [ "node: ", maybe "" toBase58Text $ _node_identity node
          , "@", _node_address node
          , " BAKER STATE:" , showForkStatus status
          , " for block:", toBase58Text bakedHash
          , " @ ",  T.pack $ show bakedTime
          , "\n"
          ]

validateForkyBlocks :: Applicative f => ([ForkInfoF ()] -> f ()) -> [ForkInfoF e] -> f ()
validateForkyBlocks f xs = case traverse (onBadForkState pure) xs of
  Success _ -> f []
  Failure bad -> f bad
