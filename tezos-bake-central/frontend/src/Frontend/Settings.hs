{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Settings where

import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (lift)
import Data.Function (on)
import Data.List (intersperse)
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MMap
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Form.Checks as Check
import GHCJS.DOM.Types (MonadJSM)
import Prelude hiding (log)
import Reflex.Dom.Core
import Reflex.Dom.Form.FieldWriter (tellFieldErr, withFormFieldsErr)
import qualified Reflex.Dom.Form.Validators as Validator
import Reflex.Dom.Form.Widgets (formItem, formItem', validatedInput)
import qualified Reflex.Dom.SemanticUI as SemUi
import qualified Reflex.Dom.TextField as Txt
import Rhyolite.Api (public)
import Rhyolite.Frontend.App (MonadRhyoliteFrontendWidget)
import Rhyolite.Schema (Email)
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
import Frontend.Watch

mailServerForm
  :: ( MonadRhyoliteFrontendWidget Bake t m
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => (MailServerView, [Email]) -> m (Event t ((MailServerView, Text), [Email]))
mailServerForm (srv0, emails0) = do
  (form, save) <- formWithSubmit $ do
    srvform <- serverFields
    mailform <- mailNotificationOptions
    let form = (liftA2 . liftA2) (,) srvform mailform
    elDynAttr "button"
      (ffor (isRight <$> form) $ \s -> "type"=:"submit"
        <> "class"=:("ui tiny primary submit button" <> if s then "" else " disabled")
      ) $ text "Save Email Settings"
    return form

  pure $ filterRight $ tag (current form) save

  where
    mailNotificationOptions = do
      divClass "ui medium header" $ text "Notification Recipients"

      let
        emailWidget email = do
          (remove, _) <- el' "a" $ icon "icon-x"
          (send, _) <- el' "a" $ text "Send Test Email"
          void $ requestingIdentity $ public . PublicRequest_SendTestEmail <$> (current email <@ domEvent Click send)
          pure $ domEvent Click remove

      listInput "Add email address" (isRight . Check.email) emailWidget emails0

    serverFields = withFormFieldsErr (srv0, "") $ do
      divClass "three fields" $ do
        tellFieldErr (_1 . mailServerView_hostName) <=< formItem' "required eight wide"
          $ validatedInput Validator.validateText
          $ defTxt "Host" & Txt.setInitial (_mailServerView_hostName srv0)

        tellFieldErr (_1 . mailServerView_portNumber) <=< formItem' "required four wide"
          $ validatedInput (Validator.validateNumeric "port" (Just 0, Just 65535) (Just 1))
          $ defTxt "Port" & Txt.setInitial (tshow $ _mailServerView_portNumber srv0)

        tellFieldErr (_1 . mailServerView_smtpProtocol) <=< formItem' "required four wide"
          $ fmap (fmap (maybe (Left "Please select a protocol") Right) . SemUi._dropdown_value)
          $ do
            labeled "Protocol"
            SemUi.dropdown (def & SemUi.dropdownConfig_placeholder .~ "Protocol"
                                & SemUi.dropdownConfig_fluid SemUi.|~ True)
              (Just $ _mailServerView_smtpProtocol srv0)
              never
              $ SemUi.TaggedStatic
              $ SmtpProtocol_Plain=:text "Plain"
              <> SmtpProtocol_Ssl=:text "SSL"
              <> SmtpProtocol_Starttls=:text "STARTTLS"

      divClass "two fields" $ do
        tellFieldErr (_1 . mailServerView_userName) <=< formItem
          $ validatedInput (Validator.optionalWith "" id Validator.validateText)
          $ defTxt "User name" & Txt.setInitial (_mailServerView_userName srv0)

        tellFieldErr _2 <=< formItem
          $ validatedInput (Validator.optionalWith "" id validatePassword)
          $ defTxt "Password"

    validatePassword = Validator.Validator (\x -> if T.null x then Left "Please enter a password" else Right x) Txt.setPasswordType
    defTxt txt = def & Txt.addLabel (labeled txt) & Txt.setPlaceholder txt
    labeled = el "label" . text

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
      [ ("Email","letter",mailServerOptions)
      , ("Telegram","telegram",telegramOptions)
      ]

  where
    notificationSection :: (Text, Text, m ()) -> m ()
    notificationSection (name, iconName, content) =
      divClass "notifications-subsection" $ do
        toggleSwitch <- SemUi.header
          (def
            & SemUi.headerConfig_size SemUi.|?~ SemUi.H4
            )
          $ do
              flip SemUi.checkbox
                (def
                  & SemUi.checkboxConfig_type SemUi.|?~ SemUi.Toggle
                  & SemUi.checkboxConfig_setValue . SemUi.initial .~ True
                  )
                $ do
                    icon ("icon-" <> iconName)
                    text name
        dyn_ $ ffor (toggleSwitch ^. SemUi.checkbox_value) $ \case
          False -> divClass "purpose" $ text $ name <> " notifications are turned off"
          True -> content

    telegramOptions = do
      divClass "purpose" $ text "Use a Telegram Bot to send alerts."
      Telegram.inlineSettings

    mailServerOptions :: m ()
    mailServerOptions = do
      divClass "ui medium header" $ text "SMTP Mail Server"
      mailServer <- watchMailServer
      notificatees <- watchNotificatees

      dyn_ $ ffor2 mailServer notificatees $ \cfg ns0 -> do
        let srv0 = fromMaybe (MailServerView "" 587 SmtpProtocol_Ssl "") cfg
        updatedForm <- mailServerForm (srv0, MMap.elems ns0)
        requestingIdentity $ public . (\((srv, pass), ns) -> PublicRequest_SetMailServerConfig srv ns pass) <$> updatedForm

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

-- | Control that allows the user to build a list of items.
listInput :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
          => Text -- ^ Placeholder for input
          -> (Text -> Bool) -- ^ Input validation
          -> (Dynamic t Text -> m (Event t ())) -- ^ Widget builder for each item in the list
          -> [Text] -- ^ Initial items
          -> m (Dynamic t (Either Text [Text])) -- ^ Items in list
listInput ph validate itemWidget items0 = mdo
  let insert is = if any T.null is
                  then is
                  else Map.insert (succ . maybe 0 fst $ Map.lookupMax is) "" is

      validation t = if T.null t
                     then Right t
                     else submission t

      submission = Check.satisfies validate "Invalid email address"

  items' <- holdUniqDyn <=< holdDyn (Map.fromList $ zip [0::Int ..] items0) $ leftmost
    [ insert <$> updated items
    , attachWith (flip Map.delete) (current items) $ fmap getFirst delete
    ]

  (items, delete) <- runEventWriterT $ fmap joinDynThroughMap $ listWithKey items' $ \k v -> divClass "ui fields" $ do
    divClass "ui inline field" $ mdo
      postBuild <- getPostBuild
      txt <- fmap value $ inputElement $ def
        & initialAttributes .~ ("placeholder" =: ph)
        & inputElementConfig_setValue .~ (current v <@ postBuild)
        & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ updated validationAttrs

      let
        validationAttrs = ffor (validation <$> txt) $ mapKeysToAttributeName . ("class" =:) . \case
          Left _ -> Just "invalid"
          Right _ -> Nothing

      dyn_ $ ffor (submission <$> txt) $ \case
        Left _ -> blank
        Right _ -> do
          del <- lift $ itemWidget txt
          tellEvent $ First k <$ del

      pure txt

  pure $ fmap sequence $ fmap submission . ffilter (not . T.null) . Map.elems <$> items
