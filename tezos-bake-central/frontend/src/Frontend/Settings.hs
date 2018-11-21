{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Settings where

import Control.Monad (guard)
import Data.Function (on)
import Data.Functor.Infix
import Data.List (intersperse)
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MMap
import qualified Data.Text as T
import Data.Version (showVersion)
import GHCJS.DOM.Types (MonadJSM)
import Prelude hiding (log)
import Reflex.Dom.Core
import qualified Reflex.Dom.Form.Validators as Validator
import qualified Reflex.Dom.SemanticUI as SemUi
import Rhyolite.Api (public)
import Rhyolite.Frontend.App (MonadRhyoliteFrontendWidget)
import qualified Text.URI as Uri

import Tezos.Types

import Common.Api
import Common.App
import Common.Config (HasFrontendConfig (frontendConfig), frontendConfig_appVersion,
                      frontendConfig_upgradeBranch)
import Common.Schema hiding (Event)
import ExtraPrelude
import Frontend.Common
import Frontend.Modal.Class (HasModal (ModalM, tellModal))
import qualified Frontend.Settings.Telegram as Telegram
import qualified Frontend.Settings.Mail as Mail
import Frontend.Watch

type EditFun t cfg m = Dynamic t (Maybe cfg) -> m (Event t ()) -- state change

data NotificationCfg m t = forall cfg. NotificationCfg
  { _notificationCfg_name :: Text
  , _notificationCfg_description :: Text
  , _notificationCfg_iconName :: Text
  , _notificationCfg_view :: Dynamic t cfg -> m (Event t ())
  , -- | The edit setting widget. Either is used to control whether editing
    -- should switch what is displayed or open a modal.
    _notificationCfg_edit :: Either
      (EditFun t cfg (ModalM m)) -- Left for modal
      (EditFun t cfg m)          -- Right for non Modal
  , _notificationCfg_method :: AlertNotificationMethod
  , -- Outer Maybe means not loaded
    _notificationCfg_watchCfg :: m (Dynamic t (Maybe (Maybe cfg)))
  , _notificationCfg_getEnabled :: cfg -> Bool
  }

-- | If editing is instead a modal 'SettingsRoute_Edit' is not used.
--
-- TODO incorporate into consistent routing framework somehow. Keep in mind that
-- there are two settings sections, mail and route, so the page route is
-- '(SettingsRoute, SettingsRoute)'.
data SettingsRoute t cfg
  = SettingsRoute_Button
  | SettingsRoute_Disabled
  | SettingsRoute_View (Dynamic t cfg)
  | SettingsRoute_Edit

settingsTab
  :: forall r t m.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadJSM (Performable m)
    , MonadJSM m
    , MonadReader r m, HasFrontendConfig r, HasTimer t r, HasTimeZone r
    , HasModal t m, MonadRhyoliteFrontendWidget Bake t (ModalM m)
    )
  => m ()
settingsTab = do
  divClass "version-section" $ do
    currentVersion <- asks (^. frontendConfig . frontendConfig_appVersion)
    divClass "soft-heading" $ text $ "Kiln Version " <> T.pack (showVersion currentVersion)

    enableUpgradeCheck <- isJust <$> asks (^. frontendConfig . frontendConfig_upgradeBranch)
    when enableUpgradeCheck upgradeOptions

  divClass "notifications-section" $ do
    SemUi.header
      (def
        & SemUi.headerConfig_size SemUi.|?~ SemUi.H3
        )
      $ text "Notifications"

    sequence_ $ intersperse (SemUi.divider def) $ map notificationSection
      [ NotificationCfg
        { _notificationCfg_name = "Email"
        , _notificationCfg_description = "Use your own email server to send alerts."
        , _notificationCfg_iconName = "letter"
        , _notificationCfg_view = Mail.viewCfg
        , _notificationCfg_edit = Right Mail.editCfg
        , _notificationCfg_method = AlertNotificationMethod_Email
        , _notificationCfg_watchCfg = watchMailServer
        , _notificationCfg_getEnabled = _mailServerView_enabled

        }
      , NotificationCfg
        { _notificationCfg_name = "Telegram"
        , _notificationCfg_description = "Use a Telegram Bot to send alerts."
        , _notificationCfg_iconName = "telegram"
        , _notificationCfg_view = Telegram.viewCfg
        , _notificationCfg_edit = Left Telegram.editCfg
        , _notificationCfg_method = AlertNotificationMethod_Telegram
        , _notificationCfg_watchCfg = watchTelegramConfig
        , _notificationCfg_getEnabled = _telegramConfig_enabled
        }
      ]
  where
    notificationSection :: NotificationCfg m t -> m ()
    notificationSection (NotificationCfg name descr iconName viewCfg editCfg method watchCfg (getEnabled :: cfg -> Bool)) =
      divClass "notifications-subsection" $ do
        dmdmCfg <- maybeDyn =<< watchCfg
        dyn_ $ ffor dmdmCfg $ \case
          Nothing -> divClass "ui active centered inline text loader"
            $ text $ name <> " notification settings loading."
          Just dmCfg' -> do
            -- If the settings are invalid, we turn the `Just cfg` to
            -- nothing. This conflates invalid saved data with no saved
            -- data.
            let
              headerIconText = do
                icon ("icon-" <> iconName)
                text name
            dmdCfg <- maybeDyn dmCfg'
            SemUi.header
              (def
                & SemUi.headerConfig_size SemUi.|?~ SemUi.H4
                )
              $ dyn_ $ ffor (getEnabled <$$$> dmdCfg) $ \case
                -- If nothing is set, return nothing
                Nothing -> headerIconText
                -- If something is set, the enable toggle should appear and
                -- return the value of that config.
                Just (dEnabled :: Dynamic t Bool) -> do
                  pb <- getPostBuild
                  let setVal = leftmost [updated dEnabled, tag (current dEnabled) pb]
                  toggleSwitch <- SemUi.checkbox
                    headerIconText
                    (def
                      & SemUi.checkboxConfig_type SemUi.|?~ SemUi.Toggle
                      & SemUi.checkboxConfig_setValue . SemUi.initial .~ True
                      & SemUi.checkboxConfig_setValue . SemUi.event .~ Just setVal
                      )
                  -- Set enabled state based on toggle.
                  statuses <- requestingIdentity $ fmap (public . PublicRequest_SetAlertNotificationMethodEnabled method) $
                    updated $ toggleSwitch ^. SemUi.checkbox_value
                  void $ runWithReplace (pure ()) $ ffor statuses $ \case
                    True -> pure ()
                    False -> fail $ show $ "\
                      \Can't enable unconfigured " <> iconName <> " notifications. \
                      \It is a bug that the user even had a toggle to click in this case."
            rec
              let fromEnabled :: Dynamic t (SettingsRoute t cfg) = dmdCfg >>= \case
                    Nothing -> pure SettingsRoute_Button
                    Just dCfg -> ffor (getEnabled <$> dCfg) $ \case
                      True -> SettingsRoute_View dCfg
                      False -> SettingsRoute_Disabled
              (eEdit :: Event t Bool) <- (=<<) (switchHold never) $ dyn $ ffor route $ \case
                SettingsRoute_Button -> do
                  divClass "notification-settings-description" $ text descr
                  True <$$ uiButton "primary" ("Connect " <> name)
                SettingsRoute_View dcfg -> do
                  divClass "notification-settings-description" $ text descr
                  True <$$ viewCfg dcfg
                SettingsRoute_Edit -> do
                  divClass "notification-settings-description" $ text descr
                  editCfg' <- case editCfg of
                    Left _ -> fail "don't switch routes for modal version"
                    Right ec -> pure ec
                  False <$$ editCfg' dmCfg'
                SettingsRoute_Disabled -> do
                  divClass "notification-settings-description" $ text $ name <> " notifications are turned off"
                  pure never
              (dEdit :: Dynamic t Bool) <- holdDyn False eEdit
              -- Implement the switch case. There is no modal so the button
              -- instead sets the edit route.
              let route = if isRight editCfg
                    then ffor2 fromEnabled dEdit $ curry $ \case
                      (SettingsRoute_Disabled, _) -> SettingsRoute_Disabled
                      (normal, False) -> normal
                      (_, True) -> SettingsRoute_Edit
                    else fromEnabled
              -- Implement the modal case. The button triggers the modal and
              -- does not change the route.
              case editCfg of
                Right _ -> pure ()
                Left editCfg' -> tellModal $ (fmapMaybe guard eEdit $>) $
                  cancelableModal $ \close -> do
                    finish <- editCfg' dmCfg'
                    pure $ leftmost [finish, close]
            pure ()

    _clientsOptions :: m ()
    _clientsOptions = void $ do
      divClass "ui medium header" $ text "Clients"
      elClass "table" "ui celled striped compact table" $ do
        clients <- watchClientAddresses -- TODO
        _ <- listWithKey (coerce <$> clients) $ \_ dName -> el "tr" $ do
          el "td" $ dynText $ Uri.render <$> dName
          el "td" $ do
            eRemove <- buttonWithInfo "Remove" "Stop monitoring this client. It will continue running."
            requestingIdentity $ public . PublicRequest_RemoveClient <$> tag (current dName) eRemove

        addE <- aliasedInputForm validateUri blank never "Add Baker" "Begin monitoring the baker at the address entered." "http://[host][:port]"
        void $ requestingIdentity $ ffor addE $ \(addr,alias) -> public (PublicRequest_AddClient addr alias)

    _delegatesOptions = do
      divClass "ui medium header" $ text "Delegates"
      elClass "table" "ui celled striped compact table" $ do
        delegates <- watchDelegatePublicKeyHashes
        _ <- listWithKey (Map.fromSet (const ()) <$> delegates) $ \pkh _ -> el "tr" $ do
          el "td" $ publicKeyHashLink pkh
          el "td" $ do
            eRemove <- buttonWithInfo "Remove" "Stop monitoring this delegate."
            requestingIdentity $ public . PublicRequest_RemoveDelegate <$> tag (pure pkh) eRemove

        addE <- aliasedInputForm (Validator.Validator (first tshow . tryReadPublicKeyHashText) id) blank never "Add Delegate" "Begin monitoring wallet address entered." "tz..."
        void $ requestingIdentity $ ffor addE $ \(pkh,alias) -> public (PublicRequest_AddDelegate pkh alias)

    upgradeOptions = do
      currentVersion <- asks (^. frontendConfig . frontendConfig_appVersion)
      upstreamVersion <- watchUpstreamVersion

      elClass "p" "check-for-updates" $ do
        (aEl, _) <- el' "a" $ text "Check for updates"
        rec
          let submit = gate (not <$> current isLoading) $ domEvent Click aEl
          (isLoading, _gotResponse) <- formIsLoading ((<) `on` (^? _Just . upstreamVersion_updated)) upstreamVersion submit
        _ <- requestingIdentity $ public PublicRequest_CheckForUpgrade <$ submit

        dyn_ $ ffor2 upstreamVersion isLoading $ \v' loading -> case loading of
          True -> divClass "ui tiny active inline loader" blank *> text " Checking for updates..."
          False -> case v' of
            Just UpstreamVersion { _upstreamVersion_error = Just _e } -> text "Unable to reach update server."
            Just UpstreamVersion { _upstreamVersion_version = Just v, _upstreamVersion_updated = updatedTime } ->
              if v > currentVersion
              then changelogLink "" v $
                text ("Version " <> T.pack (showVersion v) <> " Available ") *> icon "icon-pop-out"
              else
                text "Up to date as of " *> localHumanizedTimestamp (pure updatedTime)
            _ -> blank
