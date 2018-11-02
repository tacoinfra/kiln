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
  :: ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     )
  => MailServerView -> m (Event t (MailServerView, Text))
mailServerForm frm0 = do
  (form, save) <- formWithSubmit $ do
    form <- fields
    elDynAttr "button"
      (ffor (isRight <$> form) $ \s -> "type"=:"submit"
        <> "class"=:("ui tiny primary submit button" <> if s then "" else " disabled")
      ) $ text "Save"
    return form

  pure $ filterRight $ tag (current form) save

  where
    fields = withFormFieldsErr (frm0, "") $ do
      divClass "three fields" $ do
        tellFieldErr (_1 . mailServerView_hostName) <=< formItem' "required eight wide"
          $ validatedInput Validator.validateText
          $ defTxt "Host" & Txt.setInitial (_mailServerView_hostName frm0)

        tellFieldErr (_1 . mailServerView_portNumber) <=< formItem' "required four wide"
          $ validatedInput (Validator.validateNumeric "port" (Just 0, Just 65535) (Just 1))
          $ defTxt "Port" & Txt.setInitial (tshow $ _mailServerView_portNumber frm0)

        tellFieldErr (_1 . mailServerView_smtpProtocol) <=< formItem' "required four wide"
          $ fmap (fmap (maybe (Left "Please select a protocol") Right) . SemUi._dropdown_value)
          $ do
            labeled "Protocol"
            SemUi.dropdown (def & SemUi.dropdownConfig_placeholder .~ "Protocol"
                                & SemUi.dropdownConfig_fluid SemUi.|~ True)
              (Just $ _mailServerView_smtpProtocol frm0)
              $ SemUi.TaggedStatic
              $ SmtpProtocol_Plain=:text "Plain"
              <> SmtpProtocol_Ssl=:text "SSL"
              <> SmtpProtocol_Starttls=:text "STARTTLS"

      divClass "two fields" $ do
        tellFieldErr (_1 . mailServerView_userName) <=< formItem
          $ validatedInput (Validator.optionalWith "" id Validator.validateText)
          $ defTxt "User name" & Txt.setInitial (_mailServerView_userName frm0)

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

    notificationOptions = do
      divClass "ui medium header" $ text "Notification Recipients"
      notificatees <- watchNotificatees

      let
        emailWidget email = do
          dynText email
          text " "
          ev <- uiButton "mini compact orange" "Send test"
          void $ requestingIdentity $ public . PublicRequest_SendTestEmail <$> tag (current email) ev

      rec (addN, removeN) <- listInput "user@example.com" (isRight . Check.email) emailWidget notificatees (Right "" <$ addedN)
          addedN <- requestingIdentity . ffor addN $ \email -> public (PublicRequest_AddNotificatee email)
          _ <- requestingIdentity . ffor removeN $ \(_, email) -> public (PublicRequest_RemoveNotificatee email)

      pure ()

    mailServerOptions = do
      divClass "ui medium header" $ text "SMTP Mail Server"
      mailServer <- watchMailServer
      dyn_ $ ffor mailServer $ \cfg -> do
        let form0 = fromMaybe (MailServerView "" 587 SmtpProtocol_Ssl "") cfg
        updatedForm <- mailServerForm form0
        requestingIdentity $ public . uncurry PublicRequest_SetMailServerConfig <$> updatedForm
      notificationOptions

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
listInput :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Ord k)
          => Text -- ^ Placeholder for input
          -> (Text -> Bool) -- ^ Input validation
          -> (Dynamic t Text -> m ()) -- ^ Widget builder for each item in the list
          -> Dynamic t (MonoidalMap k Text) -- ^ Items in list
          -> Event t (Either [Text] Text) -- ^ Event of error messages or successful submission
          -> m (Event t Text, Event t (k, Text)) -- ^ Add item event, remove item event
listInput ph validate itemWidget items rsp = divClass "list-input" $ do
  rec (i, addClick) <- divClass "item-input" $ do
        itemInput <- inputElement $ def
          & initialAttributes .~ ("placeholder" =: ph)
          & inputElementConfig_setValue .~ ("" <$ fmapMaybe (^? _Right) rsp)
          & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ validationAttrs
        addItemClick <- fmap (domEvent Click . fst) $ elClass' "span" "add-button" $ elClass "i" "fa fa-plus-circle fa-fw" blank
        return (itemInput, addItemClick)
      let v = value i
          validationResults = leftmost
            [ (\v' -> if T.null v' then Left () else Right (validate v')) <$> updated v
            , Right . isJust . preview _Right <$> rsp
            ]
          validationAttrs = ffor validationResults $ \r -> mapKeysToAttributeName $ case r of
            Left () -> "class" =: Nothing
            Right True -> "class" =: Nothing
            Right False -> "class" =: Just "invalid"
          submit = tag (current v) $ leftmost
            [ () <$ ffilter ((==Enter) . keyCodeLookup . fromIntegral) (domEvent Keypress i)
            , addClick
            ]
      widgetHold_ blank $ ffor rsp $ \case
        Left errs -> for_ errs $ elClass "div" "modal-content__text-input-error" . text
        Right success -> elClass "div" "modal-content__text-input-success" $ text success
      remove <- fmap (fmap (leftmost . Map.elems)) $ elClass "ul" "list-input-items" $
        listWithKey (coerce <$> items) $ \k t -> el "li" $ do
          el "span" $ itemWidget t
          fmap ((,) k) . tag (current t) . domEvent Click . fst <$> el' "span" (elClass "i" "fa fa-fw fa-times-circle" blank)
  return (ffilter validate submit, switch . current $ remove)
