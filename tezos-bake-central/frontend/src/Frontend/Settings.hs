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
import Frontend.Modal.Class (HasModal (ModalM))
import qualified Frontend.Settings.Telegram as Telegram
import qualified Frontend.Settings.Mail as Mail
import Frontend.Watch

data NotificationCfg m t = forall cfg. NotificationCfg
  { _notificationCfg_name :: Text
  , _notificationCfg_iconName :: Text
  , _notificationCfg_content :: Dynamic t (Maybe cfg) -> m ()
  , _notificationCfg_method :: AlertNotificationMethod
  , -- Outer Maybe means not loaded
    _notificationCfg_watchCfg :: m (Dynamic t (Maybe (Maybe cfg)))
  , _notificationCfg_getEnabled :: cfg -> Bool
  }

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

    sequence_ $ intersperse (SemUi.divider def) $ map notificationSection $
      [ NotificationCfg
        { _notificationCfg_name = "Email"
        , _notificationCfg_iconName = "letter"
        , _notificationCfg_content = mailServerOptions
        , _notificationCfg_method = AlertNotificationMethod_Email
        , _notificationCfg_watchCfg = watchMailServer
        , _notificationCfg_getEnabled = _mailServerView_enabled
        }
      , NotificationCfg
        { _notificationCfg_name = "Telegram"
        , _notificationCfg_iconName = "telegram"
        , _notificationCfg_content = telegramOptions
        , _notificationCfg_method = AlertNotificationMethod_Telegram
        , _notificationCfg_watchCfg = watchTelegramConfig
        , _notificationCfg_getEnabled = _telegramConfig_enabled
        }
      ]
  where
    notificationSection :: NotificationCfg m t -> m ()
    notificationSection (NotificationCfg name iconName content method watchCfg getEnabled) =
      divClass "notifications-subsection" $ do
        dmdmCfg <- maybeDyn =<< watchCfg
        dyn_ $ ffor dmdmCfg $ \case
          Nothing -> divClass "ui active centered inline text loader"
            $ text $ name <> " notification settings loading."
          Just dmCfg -> do
            (showSettings :: Dynamic t Bool) <- SemUi.header
              (def
                & SemUi.headerConfig_size SemUi.|?~ SemUi.H4
                )
              $ do
                let headerIconText = do
                      icon ("icon-" <> iconName)
                      text name
                    initialShowHeader = True
                ddEnabled <- maybeDyn $ getEnabled <$$> dmCfg
                checkEvent <- dyn $ ffor ddEnabled $ \case
                  -- If nothing is set, the settings should always be available.
                  Nothing -> pure initialShowHeader <$ headerIconText
                  -- If something is set, the enable toggle should appear and the
                  -- settings should only show up if toggle is enabled.
                  Just (dEnabled :: Dynamic t Bool) -> do
                    pb <- getPostBuild
                    let setVal = leftmost [updated dEnabled, tag (current dEnabled) pb]
                    toggleSwitch <- flip SemUi.checkbox
                      (def
                        & SemUi.checkboxConfig_type SemUi.|?~ SemUi.Toggle
                        & SemUi.checkboxConfig_setValue . SemUi.initial .~ True
                        & SemUi.checkboxConfig_setValue . SemUi.event .~ Just setVal
                        )
                      $ headerIconText
                    -- Set enabled state based on toggle.
                    statuses <- requestingIdentity $ fmap (public . PublicRequest_SetAlertNotificationMethodEnabled method) $
                      updated $ toggleSwitch ^. SemUi.checkbox_value
                    _ <- runWithReplace (pure ()) $ ffor statuses $ \case
                      True -> pure ()
                      False -> fail $ show $ "\
\Can't enable unconfigured " <> iconName <> " notifications. \
\It is a bug that the user even had a toggle to click in this case."
                    pure $ toggleSwitch ^. SemUi.checkbox_value
                join <$> holdDyn (pure initialShowHeader) checkEvent
            dyn_ $ ffor showSettings $ \case
              False -> divClass "purpose" $ text $ name <> " notifications are turned off"
              True -> content dmCfg

    telegramOptions cfg = do
      divClass "purpose" $ text "Use a Telegram Bot to send alerts."
      Telegram.inlineSettings cfg

    mailServerOptions :: Dynamic t (Maybe MailServerView) -> m ()
    mailServerOptions mailServer = do
      divClass "notification-settings-description" $ text "Use your own email server to send alerts."
      notificatees <- watchNotificatees

      dyn_ $ ffor2 mailServer notificatees $ \cfg ns0 -> do
        let srv0 = fromMaybe (MailServerView "" 587 SmtpProtocol_Ssl "" True) cfg
        updatedForm <- Mail.mailServerForm (srv0, MMap.elems ns0)
        requestingIdentity $ public . (\((srv, pass), ns) -> PublicRequest_SetMailServerConfig srv ns pass ) <$> updatedForm

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

        addE <- aliasedInputForm validateUri "Add Baker" "Begin monitoring the baker at the address entered." "http://[host][:port]"
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

        addE <- aliasedInputForm (Validator.Validator (first tshow . tryReadPublicKeyHashText) id) "Add Delegate" "Begin monitoring wallet address entered." "tz..."
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
