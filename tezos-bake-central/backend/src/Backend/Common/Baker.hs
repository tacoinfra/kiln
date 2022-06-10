{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Backend.Common.Baker where

import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (maybeToList)
import Database.Groundhog.Postgresql
import Database.Id.Class
import Database.Id.Groundhog
import Tezos.Types (ChainId, PublicKeyHash)

import Backend.Schema
import Common.App
import Common.Schema
import ExtraPrelude

addBakerImpl :: (Monad m, PersistBackend m) => PublicKeyHash -> Maybe Text -> m ()
addBakerImpl pkh alias = do
  existingIds :: [Id Baker] <- fmap toId <$> project BakerKey (Baker_publicKeyHashField ==. pkh)
  let newVal = BakerData
        { _bakerData_alias = alias
        }
  case nonEmpty existingIds of
    Nothing -> void $ insert $ Baker
      { _baker_publicKeyHash = pkh
      , _baker_data = DeletableRow
        { _deletableRow_data = newVal
        , _deletableRow_deleted = False
        }
      }
    Just bIds -> for_ bIds $ \bId ->
      update [ Baker_dataField ~> DeletableRow_deletedSelector =. False
             , Baker_dataField ~> DeletableRow_dataSelector ~> BakerData_aliasSelector =. alias
             ]
             (BakerKey ==. fromId bId)
  notify NotifyTag_Baker (Id pkh, Just newVal)

class IsBakerExtraArgs a where
  toBakerExtraArgs :: a -> PublicKeyHash -> ChainId -> BakerExtraArgs

-- TODO [#136]: create 'option' based on chain id or make it protocol dependent in another way
instance IsBakerExtraArgs LiquidityBakingToggleVote where
  toBakerExtraArgs lqdtyToggle pkh chainId = BakerExtraArgs
    { _bakerExtraArgs_publicKeyHash = pkh
    , _bakerExtraArgs_chainId = chainId
    , _bakerExtraArgs_option = "--liquidity-baking-toggle-vote"
    , _bakerExtraArgs_value = Just optionValue
    }
    where
      optionValue = case lqdtyToggle of
        LiquidityBakingToggleVote_On   -> "on"
        LiquidityBakingToggleVote_Off  -> "off"
        LiquidityBakingToggleVote_Pass -> "pass"

toCmdArg :: BakerExtraArgs -> [Text]
toCmdArg BakerExtraArgs{..} = _bakerExtraArgs_option : maybeToList _bakerExtraArgs_value
