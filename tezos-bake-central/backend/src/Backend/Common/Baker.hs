{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Backend.Common.Baker where

import Control.Monad.Logger (MonadLoggerIO, logWarn)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import Database.Id.Class
import Database.Id.Groundhog
import Rhyolite.Backend.DB (project1)
import Tezos.Types (ChainId, PublicKeyHash)

import Backend.Config (AppConfig (..), HasAppConfig (..))
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

startBakerDaemon
  :: ( Monad m
     , PersistBackend m
     , SqlDb (PhantomDb m)
     )
  => m ()
startBakerDaemon = updateBakerDaemon ProcessControl_Run

stopBakerDaemon
  :: ( Monad m
     , PersistBackend m
     , SqlDb (PhantomDb m)
     )
  => m ()
stopBakerDaemon = updateBakerDaemon ProcessControl_Stop

restartBakerDaemon
  :: ( Monad m
     , PersistBackend m
     , SqlDb (PhantomDb m)
     )
  => m ()
restartBakerDaemon = updateBakerDaemon ProcessControl_Restart

updateBakerDaemon
  :: ( Monad m
     , PersistBackend m
     , SqlDb (PhantomDb m)
     )
  => ProcessControl -> m ()
updateBakerDaemon control = do
  project1 (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty
    >>= traverse_ (\bdid -> do
      let bPid = _bakerDaemonInternalData_bakerProcessData bdid
      update
        [ ProcessData_controlField =. control
        , ProcessData_errorLogField =. (Nothing :: Maybe Text)
        ] (AutoKeyField ==. fromId bPid))

getKilnBakerCustomArgs
  :: ( MonadLoggerIO m
     , MonadReader e m
     , HasAppConfig e
     )
  => m [String]
getKilnBakerCustomArgs = do
  appConfig <- asks (view getAppConfig)
  let fullArgs = maybe [] (words . T.unpack) (_appConfig_kilnBakerCustomArgs appConfig)
  case span (/= liquidityBakingArg) fullArgs of
    (xs, _ : y : ys) | isCorrectLiquidityBakingValue y -> do
      $(logWarn) $ "'liquidity-baking-toggle-vote' baker custom argument"
        <> " was ignored because it's set on Kiln UI"
      pure $ xs ++ ys
    _ -> pure fullArgs
  where
    liquidityBakingArg = "--liquidity-baking-toggle-vote"
    isCorrectLiquidityBakingValue s = s == "on" || s == "off" || s == "pass"
