{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Settings.Mail
  ( viewCfg
  , editCfg
  ) where

import Control.Monad (guard)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first, second)
import Data.Functor.Infix
import Data.Universe (universeF)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Form.Checks as Check
import GHCJS.DOM.Types (MonadJSM)
import Prelude hiding (log)
import Reflex.Dom.Core
import Reflex.Dom.Form.FieldWriter (tellFieldErr, withFormFieldsErr)
import qualified Reflex.Dom.Form.Validators as Validator
import Reflex.Dom.Form.Widgets (formItem', validatedInput)
import qualified Reflex.Dom.SemanticUI as SemUi
import qualified Reflex.Dom.TextField as Txt
import Rhyolite.Api (public)
import Rhyolite.Schema (Email)

import Common.Api
import Common.App
import Common.Schema
import ExtraPrelude
import Frontend.Common

renderProto :: SmtpProtocol -> Text
renderProto = \case
  SmtpProtocol_Plain -> "Plain"
  SmtpProtocol_Ssl -> "SSL"
  SmtpProtocol_Starttls -> "STARTTLS"

viewCfg
  :: MonadAppWidget t m
  => Dynamic t MailServerView
  -> m (Event t ())
viewCfg dMsv = do --never <$ text "TODO Email View"
  (reopener, _) <- elClass "p" "edit-link" $ do
    el' "a" $ text "Edit Settings"

  elClass "table" "settings-table" $ do
    el "tr" $ do
      el "th" $ text "Email Server"
      el "th" $ text "Username"
      el "th" $ text "Password"
    el "tr" $ do
      el "td" $ dynText $ ffor dMsv $ \msv ->
        _mailServerView_hostName msv
        <> ":"
        <> T.pack (show $ _mailServerView_portNumber msv)
        <> " "
        <> renderProto (_mailServerView_smtpProtocol msv)
      el "td" $ dynText $ _mailServerView_userName <$> dMsv
      el "td" $ text abstractPassword

  elClass "table" "settings-table" $ do
    el "tr" $ do
      elAttr "th" ("rowspan" =: "0") $ text "Recipients"
    dyn_ $ ffor (_mailServerView_notificatees <$> dMsv) $ mapM_ $ \notificatee ->
      el "tr" $ do
        elClass "td" "visually-second" $ text notificatee

  return $ domEvent Click reopener

editCfg
  :: (MonadAppWidget t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , Prerender js t m
     )
  => Dynamic t (Maybe MailServerView) -> m (Event t ())
editCfg mailServer = do
  eeClose <- dyn $ ffor mailServer $ \cfg -> do
    let ns0 = foldMap _mailServerView_notificatees cfg
    let srv0 = fromMaybe (MailServerView "" 587 SmtpProtocol_Ssl "" True ns0) cfg
    (updatedForm, cancelled) <- mailServerForm (srv0, ns0)
    saved <- requestingIdentity $ public . (\((srv, pass), ns) -> PublicRequest_SetMailServerConfig srv ns pass ) <$> updatedForm
    pure $ leftmost [saved, cancelled]
  switchHold never eeClose


abstractPassword :: Text
abstractPassword = "••••••••••••"

mailServerForm
  :: ( MonadAppWidget t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , Prerender js t m
     )
  => (MailServerView, [Email]) -> m (Event t ((MailServerView, Maybe Text), [Email]), Event t ())
mailServerForm (srv0, emails0) = do
  ((form, cancel), save) <- formWithSubmit $ do
    srvform <- serverFields
    mailform <- mailNotificationOptions
    let form = (liftA2 . liftA2) (,) srvform mailform
    elDynAttr "button"
      (ffor (isRight <$> form) $ \s -> "type"=:"submit"
        <> "class"=:("ui tiny primary submit button" <> if s then "" else " disabled")
      ) $ text "Save Email Settings"
    cancel <- uiButton "tiny" "Cancel"
    return (form, cancel)

  -- TODO think about passwords forms and optional fields
  let form' = (fmap . first . second)
        (\t -> guard (t /= abstractPassword) *> Just t)
        (filterRight $ tag (current form) save)

  pure (form', cancel)

  where
    mailNotificationOptions = do
      divClass "notification-settings-description" $ text "Enter email address to receive email notifications below."

      let
        showSent = do
          icon "icon-check green"
          text "Sent!"
          return never
        sendLink = do
          (e,_) <- elClass' "a" "send-test" $ text "Send Test Email"
          return $ domEvent Click e
        emailWidget email = elClass "span" "email-buttons" $ do
          (remove, _) <- elClass' "a" "remove" $ icon "icon-x"
          rec
            send <- widgetHold sendLink $ leftmost [ showSent <$ sent
                                                   , sendLink <$ sentClear
                                                   ]
            let sendEv = switch (current send)
            sent <- requestingIdentity $ public . PublicRequest_SendTestEmail <$> (current email <@ sendEv)
            sentClear <- delay 2 sent
          pure $ domEvent Click remove

      listInput "Add email address" (isRight . Check.email) emailWidget emails0

    serverFields = withFormFieldsErr (srv0, "") $ do
      divClass "three fields" $ do
        tellFieldErr (_1 . mailServerView_hostName) <=< formItem' "required four wide"
          $ validatedInput Validator.validateText
          $ defTxt "Host"
            & Txt.setInitial (_mailServerView_hostName srv0)
            & Txt.setPlaceholder "e.g. 127.0.0.1"

        tellFieldErr (_1 . mailServerView_portNumber) <=< formItem' "required three wide"
          $ validatedInput (Validator.validateNumeric "port" (Just 0, Just 65535) (Just 1))
          $ defTxt "Port"
            & Txt.setInitial (tshow $ _mailServerView_portNumber srv0)
            & Txt.setPlaceholder "e.g. 465"

        tellFieldErr (_1 . mailServerView_smtpProtocol) <=< formItem' "required three wide"
          $ fmap (fmap (maybe (Left "Please select a protocol") Right) . SemUi._dropdown_value)
          $ do
            labeled "Protocol"
            SemUi.dropdown (def & SemUi.dropdownConfig_placeholder .~ "Protocol"
                                & SemUi.dropdownConfig_fluid SemUi.|~ True)
              (Just $ _mailServerView_smtpProtocol srv0)
              never
              $ SemUi.TaggedStatic
              $ fold
              $ ffor universeF $ \p ->
                p =: text (renderProto p)

      divClass "two fields" $ do
        tellFieldErr (_1 . mailServerView_userName) <=< formItem' "four wide"
          $ validatedInput (Validator.optionalWith "" id Validator.validateText)
          $ defTxt "User name" & Txt.setInitial (_mailServerView_userName srv0)

        tellFieldErr _2 <=< formItem' "three wide"
          $ validatedInput (Validator.optionalWith "" id validatePassword)
          $ Txt.setInitial abstractPassword
          $ defTxt "Password"

    validatePassword = Validator.Validator (\x -> if T.null x then Left "Please enter a password" else Right x) Txt.setPasswordType
    defTxt txt = def & Txt.addLabel (labeled txt) & Txt.setPlaceholder txt
    labeled = el "label" . text

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
