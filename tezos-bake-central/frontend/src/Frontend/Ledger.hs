{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Frontend.Ledger (ledgerSetupSteps) where

import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.Dependent.Sum (DSum(..), (==>))
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare
import Data.GADT.Compare.TH
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MMap
import Data.Some (Some(..), withSome)
import qualified Data.Text as T
import GHCJS.DOM.Types (MonadJSM)
import Obelisk.Generated.Static (static)
import Reflex.Dom.Core
import qualified Reflex.Dom.SemanticUI as SemUi
import qualified Reflex.Dom.Form.Validators as Validator
import qualified Reflex.Dom.TextField as Txt
import Reflex.Dom.Form.Widgets (formItem')
import Reflex.Dom.Form.Widgets (validatedInput)
import Rhyolite.Api (public)
import Rhyolite.Frontend.App (MonadRhyoliteFrontendWidget)
import Text.Read (readMaybe)
import Tezos.Types

import Common.Api
import Common.App
import Common.Schema hiding (Event)
import ExtraPrelude
import Frontend.Common
import Frontend.Watch

data LSS a where
  LSS_ConnectLedger :: LSS ()
  LSS_SelectAddress :: LSS LedgerIdentifier
  LSS_ImportAddress :: LSS (SecretKey, PublicKeyHash)
  LSS_AuthorizeLedger :: LSS (SecretKey, PublicKeyHash)
  LSS_RegisterDelegate :: LSS (SecretKey, PublicKeyHash)
  LSS_Complete :: LSS (SecretKey, PublicKeyHash)

deriveGEq ''LSS
deriveGCompare ''LSS

toLSSText :: LSS a -> Text
toLSSText = \case
  LSS_ConnectLedger -> "Connect Ledger"
  LSS_SelectAddress -> "Select Address"
  LSS_ImportAddress -> "Import Address"
  LSS_AuthorizeLedger -> "Authorize Ledger"
  LSS_RegisterDelegate -> "Register Delegate"
  LSS_Complete -> ""

data PromptResult m a where
  PromptResult_RecoverableError :: PromptResult m (m ())
  PromptResult_ClientError :: PromptResult m ClientError
  PromptResult_Interstitial :: PromptResult m (m ())
  PromptResult_Success :: PromptResult m ()

instance GEq (PromptResult m) where
  geq PromptResult_RecoverableError PromptResult_RecoverableError = Just Refl
  geq PromptResult_ClientError PromptResult_ClientError = Just Refl
  geq PromptResult_Interstitial PromptResult_Interstitial = Just Refl
  geq PromptResult_Success PromptResult_Success = Just Refl
  geq _ _ = Nothing

instance GCompare (PromptResult m) where
  gcompare PromptResult_RecoverableError PromptResult_RecoverableError = GEQ
  gcompare PromptResult_RecoverableError _ = GLT
  gcompare PromptResult_ClientError PromptResult_RecoverableError = GGT
  gcompare PromptResult_ClientError PromptResult_ClientError = GEQ
  gcompare PromptResult_ClientError _ = GLT
  gcompare PromptResult_Interstitial PromptResult_ClientError = GGT
  gcompare PromptResult_Interstitial PromptResult_RecoverableError = GGT
  gcompare PromptResult_Interstitial PromptResult_Interstitial = GEQ
  gcompare PromptResult_Interstitial _ = GLT
  gcompare PromptResult_Success PromptResult_Success = GEQ
  gcompare PromptResult_Success _ = GGT

ledgerSetupSteps :: forall t m. (MonadRhyoliteFrontendWidget Bake t m, MonadJSM (Performable m), MonadJSM m) => m (Event t (Either ClientError ()))
ledgerSetupSteps = mdo
  connectedLedger <- watchConnectedLedger
  ledgerIdentifier <- holdUniqDyn $ (>>= \cl -> _connectedLedger_bakingAppVersion cl >>= \_ -> _connectedLedger_ledgerIdentifier cl) <$> connectedLedger
  let disconnect = ffilter isNothing $ updated ledgerIdentifier
  divClass "progress" $ do
    elClass "h4" "ui header" $ do
      kilnLogo
      text "Start Baking"
    let steps = [This LSS_ConnectLedger, This LSS_SelectAddress, This LSS_ImportAddress, This LSS_AuthorizeLedger, This LSS_RegisterDelegate]
    el "ol" $ for_ steps $ \step -> do
      let attrs = ffor currentStep $ \(s :=> _) -> "class" =: case compare step (This s) of
            LT -> "done"
            EQ -> "current"
            GT -> ""
      elDynAttr "li" attrs $ do
        text $ withSome step toLSSText
        when (step == This LSS_ConnectLedger) $ dyn_ $ ffor ledgerIdentifier $ traverse_ $ \li -> divClass "extra" $ do
          elAttr "img" ("src" =: static @"images/ledger.svg") blank
          text $ unLedgerIdentifier li
        when (step == This LSS_SelectAddress) $ divClass "extra" $ do
          dynText $ ffor currentStep $ maybe "" toPublicKeyHashText . \case
            LSS_ImportAddress :=> Identity (_, pkh) -> Just pkh
            LSS_AuthorizeLedger :=> Identity (_, pkh) -> Just pkh
            LSS_RegisterDelegate :=> Identity (_, pkh) -> Just pkh
            LSS_Complete :=> Identity (_, pkh) -> Just pkh
            _ -> Nothing

  currentStep <- holdDyn (LSS_ConnectLedger :=> Identity ()) updateStep
  quitOrUpdate <- divClass "workflow" $ switchHold never <=< dyn $ ffor currentStep $ \case
    LSS_ConnectLedger :=> _ -> (fmap . fmap) (Right . (LSS_SelectAddress ==>)) (connectLedger connectedLedger)
    LSS_SelectAddress :=> Identity l -> (fmap . fmap) (Right . (LSS_ImportAddress ==>)) (selectAddress l)
    LSS_ImportAddress :=> Identity sk -> (fmap . fmap) (bimap Left $ const $ LSS_AuthorizeLedger ==> sk) (importSecretKey sk)
    LSS_AuthorizeLedger :=> Identity sk -> (fmap . fmap) (bimap Left id) (authorizeLedger sk)
    LSS_RegisterDelegate :=> Identity sk -> (fmap . fmap) (bimap Left $ const $ LSS_Complete ==> sk) (registerDelegate sk)
    LSS_Complete :=> Identity sk -> (fmap . fmap) (Left . Right) (setupComplete sk)
  let (quit :: Event t (Either ClientError ()), updateStep) = fanEither quitOrUpdate

  pure $ leftmost
    [ quit
    , Left ClientError_LedgerDisconnected <$ disconnect
    ]

doPrompt
  :: MonadRhyoliteFrontendWidget Bake t m
  => Text
  -- ^ Title
  -> m (Behavior t (Maybe (PublicRequest Bake ())))
  -- ^ Widget placed before the continue button
  -- ^ Returns request to start the prompt
  -> Text
  -- ^ Ledger prompt
  -> SecretKey
  -> (SetupState -> Maybe (DSum (PromptResult m) Identity))
  -> m (Event t (Either ClientError ()))
doPrompt title explanation prompt sk handleStep = divClass "central" $ do
  promptDyn <- watchPrompting sk
  let
    splash mError = Workflow $ do
      for_ mError $ \e -> divClass "rejected" $ do
        icon "large red icon-x"
        e
      elClass "h5" "ui header" $ text title
      req <- explanation
      continue <- uiButton "primary" "Continue"
      pure (never, attachWithMaybe (\r () -> prompting <$> r) req continue)
    prompting req = Workflow $ mdo
      _ <- runWithReplace (respondToPrompt $ text prompt) interstitial
      pb <- getPostBuild
      _ <- requestingIdentity $ public req <$ pb
      let changed = leftmost [updated promptDyn, tag (current promptDyn) pb]
          selector = fan $ fforMaybe changed $ \mss -> fmap (DMap.fromList . pure) . handleStep =<< mss
          back = select selector PromptResult_RecoverableError
          finished = leftmost
            [ Left <$> select selector PromptResult_ClientError
            , Right <$> select selector PromptResult_Success
            ]
          interstitial = select selector PromptResult_Interstitial
      pure (finished, splash . Just <$> back)
  switch . current <$> workflow (splash Nothing)

declinedError :: DomBuilder t m => m ()
declinedError = text "The Ledger prompt was rejected or timed out. Please try again."

failedError :: DomBuilder t m => m ()
failedError = text "Something went wrong"

respondToPrompt :: DomBuilder t m => m () -> m ()
respondToPrompt prompt = do
  elClass "h5" "ui header" $ do
    divClass "ui active tiny inline blue loader" blank
    text "Respond to the prompt on your Ledger Device..."
  divClass "centered explanation" $ text "Your Ledger Device should show the following prompt:"
  elClass "h6" "ui header prompt-text" prompt

importSecretKey
  :: forall t m. MonadRhyoliteFrontendWidget Bake t m
  => (SecretKey, PublicKeyHash) -> m (Event t (Either ClientError ()))
importSecretKey (sk, pkh) = doPrompt "Import address to Kiln." explanation prompt sk handleStep
  where
    explanation = do
      text "Kiln must import this address before it can bake and endorse with your Ledger Device. Your private keys will remain securely stored on the Ledger."
      pure $ pure $ Just $ PublicRequest_ImportSecretKey sk
    prompt = "Provide Public Key? Public Key Hash: " <> toPublicKeyHashText pkh
    handleStep ss
      | Just (First importStep) <- _setupState_import ss = case importStep of
        ImportSecretKeyStep_Done -> Just $ PromptResult_Success ==> ()
        ImportSecretKeyStep_Disconnected -> Just $ PromptResult_ClientError ==> ClientError_LedgerDisconnected
        ImportSecretKeyStep_Declined -> Just $ PromptResult_RecoverableError ==> declinedError
        ImportSecretKeyStep_Failed _e -> Just $ PromptResult_RecoverableError ==> failedError
        ImportSecretKeyStep_Prompting -> Nothing
      | otherwise = Nothing

authorizeLedger
  :: forall t m. MonadRhyoliteFrontendWidget Bake t m
  => (SecretKey, PublicKeyHash) -> m (Event t (Either ClientError (DSum LSS Identity)))
authorizeLedger (sk, pkh) = do
  e <- doPrompt "Authorize Ledger Device for this address." explanation prompt sk handleStep
  isRegisteredD <- fmap ((== Just SetupLedgerToBakeStep_DoneAndRegistered) . fmap getFirst . (_setupState_setup =<<)) <$> watchPrompting sk
  let (err, ok) = fanEither e
      -- Since 'ok' is also derived from same Dynamic, we need tagPromptlyDyn here
      isRegEv = tagPromptlyDyn isRegisteredD ok
  pure $ leftmost [Left <$> err, ffor isRegEv $ \r -> Right $ (if r then LSS_Complete else LSS_RegisterDelegate) ==> (sk, pkh)]
  where
    explanation = do
      text "This allows the Ledger Device to sign blocks and endorsements for the selected address automatically. It will not sign other operations such as transactions, and it will not sign blocks or endorsements it may have already signed."
      pure $ pure $ Just $ PublicRequest_SetupLedgerToBake sk
    prompt = "Setup Baking? Address: " <> toPublicKeyHashText pkh
    handleStep ss
      | Just (First setupStep) <- _setupState_setup ss = case setupStep of
        SetupLedgerToBakeStep_Done -> Just $ PromptResult_Success ==> ()
        SetupLedgerToBakeStep_DoneAndRegistered -> Just $ PromptResult_Success ==> ()
        SetupLedgerToBakeStep_Disconnected -> Just $ PromptResult_ClientError ==> ClientError_LedgerDisconnected
        SetupLedgerToBakeStep_Declined -> Just $ PromptResult_RecoverableError ==> declinedError
        SetupLedgerToBakeStep_Failed -> Just $ PromptResult_RecoverableError ==> failedError
        SetupLedgerToBakeStep_Prompting -> Nothing
        -- this shouldn't really happen, but if it does, we force the user to restart the process in order to get the appropriate error earlier on
        SetupLedgerToBakeStep_OutdatedVersion _v -> Just $ PromptResult_ClientError ==> ClientError_LedgerDisconnected
      | otherwise = Nothing

registerDelegate
  :: forall t m. MonadRhyoliteFrontendWidget Bake t m
  => (SecretKey, PublicKeyHash) -> m (Event t (Either ClientError ()))
registerDelegate (sk, pkh) = doPrompt "Register address as a delegate." explanation prompt sk handleStep
  where
    prompt = "Register as delegate? Address: " <> toPublicKeyHashText pkh
    handleStep ss
      | Just (First registerStep) <- _setupState_register ss = case registerStep of
        RegisterStep_Registered -> Just $ PromptResult_Success ==> ()
        RegisterStep_Disconnected -> Just $ PromptResult_ClientError ==> ClientError_LedgerDisconnected
        RegisterStep_Declined -> Just $ PromptResult_RecoverableError ==> declinedError
        RegisterStep_Failed -> Just $ PromptResult_RecoverableError ==> failedError
        RegisterStep_Prompting -> Nothing
        RegisterStep_WaitingForInclusion -> Just $ PromptResult_Interstitial ==> do
          elClass "h5" "ui header" $ do
            divClass "ui active tiny inline blue loader" blank
            text "Waiting for the registration operation to be included in a block..."
          divClass "centered explanation" $ text "To verify that your address has been registered as a delegate, the registration operation must be included in a block on the chain. This should usually take only a minute or two."
        RegisterStep_AlreadyRegistered -> Just $ PromptResult_Success ==> () -- we could also inform the user they didn't need to pay the fee
        RegisterStep_NodeNotReady -> Just $ PromptResult_ClientError ==> ClientError_NodeNotReady
        RegisterStep_FeeTooLow _fee -> Just $ PromptResult_RecoverableError ==> text "Fee is too low, please try again with a higher fee."
        RegisterStep_FeeTooHigh _fee -> Just $ PromptResult_RecoverableError ==> text "To avoid paying an unnecessarily high fee enter a value of 1 tez or less. We recommend trying the default minimum fee listed above."
        RegisterStep_NotEnoughFunds balance -> Just $ PromptResult_RecoverableError ==> do
          text "Balance ("
          elClass "span" "tez" $ text $ tez balance
          text ") too low to cover fee."
      | otherwise = Nothing
    explanation = do
      let minimumDefaultFee = 0.004 :: Tez
      text "The selected address must be registered as a delegate on the Tezos network in order to bake."
      divClass "start-baking-message" $ do
        icon "large orange icon-warning"
        divClass "header" $ text "There is a small transaction fee to register as a delegate."
        divClass "explanation" $ text "Registering a delegate is a blockchain transaction and thus has a fee, similar to a transaction like sending tez."
        divClass "explanation" $ do
          text "This address must have enough available Tez to pay the transaction fee to register as a delegate. We recommend using the default minimum fee of "
          text $ tez minimumDefaultFee
          text "."
        divClass "explanation" $ do
          text "You can read more about fees here: "
          let uri = "http://tezos.gitlab.io/master/protocols/003_PsddFKi3.html"
          hrefLink uri $ text uri
      text "Transaction Fee (ꜩ)"
      fee <- fmap (current . value) $ SemUi.input (def & SemUi.classes .~ "tx-fee-input") $ inputElement $ def
        & inputElementConfig_initialValue .~ T.pack (show $ getTez minimumDefaultFee)
        & initialAttributes .~ "type" =: "number" <> "step" =: "0.000001" <> "min" =: "0"
      pure $ ffor fee $ \t -> PublicRequest_RegisterKeyAsDelegate sk . Tez <$> readMaybe (T.unpack t)

connectLedger
  :: MonadRhyoliteFrontendWidget Bake t m
  => Dynamic t (Maybe ConnectedLedger) -> m (Event t LedgerIdentifier)
connectLedger connectedLedger = divClass "central" $ do
  elAttr "img" ("src" =: static @"images/ledger.svg" <> "class" =: "ledger") blank
  elClass "h5" "ui header" $ do
    divClass "ui active small inline blue loader" blank
    text "Looking for Ledger Device..."
  let checkVersion cl
        | Just v <- _connectedLedger_bakingAppVersion cl
        , Just l <- _connectedLedger_ledgerIdentifier cl
        , v >= requiredTezosBakingAppVersion
        = Just $ Right l
        | Just v <- _connectedLedger_bakingAppVersion cl
        = Just $ Left v
        | otherwise = Nothing
      (outdatedVersion, ledgerChoice) = fanEither $ fmapMaybe (checkVersion =<<) (updated connectedLedger)
  el "p" $ text $ "Connect your Ledger Device, enter the PIN and open the Tezos Baking app (version " <> requiredTezosBakingAppVersion <> " or higher)."
  _ <- runWithReplace blank $ ffor outdatedVersion $ \version -> divClass "start-baking-message" $ do
    icon "large orange icon-warning"
    divClass "header" $ text "Incompatible version of Tezos Baking."
    divClass "explanation" $ text $ "Kiln has detected a connected Ledger Device with the Tezos Baking app version " <> version <> " installed."
    divClass "explanation" $ text $ "In order to bake, you will need to use Ledger Live to update the Tezos Baking app to version " <> requiredTezosBakingAppVersion <> " or higher."
  divClass "explanation" $ do
    text "To install the Tezos Baking app:"
    el "ol" $ do
      el "li" $ do
        text "Install and open Ledger Live: "
        let uri = "https://www.ledger.com/pages/ledger-live"
        hrefLink uri $ text uri
      el "li" $ text "Navigate to Settings and turn on \"Developer Mode\""
      el "li" $ text "Go to Manager and search for \"Tezos\""
      el "li" $ text "Install the \"Tezos Baking\" app"
      el "li" $ text "Open the Tezos Baking app on your ledger"
  pure ledgerChoice

selectAddress
  :: forall t m. (MonadRhyoliteFrontendWidget Bake t m, MonadJSM (Performable m), MonadJSM m)
  => LedgerIdentifier -> m (Event t (SecretKey, PublicKeyHash))
selectAddress ledger = divClass "select-address" $ mdo
  let curves = [minBound .. maxBound] :: [SigningCurve]
      derivs = [primaryDeriv, DerivationPath ""]
      primaryDeriv = DerivationPath "0'/0'"
      secretKeys = SecretKey ledger <$> curves <*> derivs
  elClass "h5" "ui header" $ text "Select an account to bake with."
  let submitted = domEvent Submit formEl

  pb <- getPostBuild
  traverse_ (\sk -> requestingIdentity $ public (PublicRequest_ShowLedger sk) <$ pb) (reverse secretKeys)

  (formEl, selection) <- elDynAttrWithModifyEvent' preventDefault Submit "form" ((\e -> "class" =: ("ui form" <> if e then " error" else "")) <$> hasError) $ mdo
    let accountItem :: SecretKey -> Dynamic t (Maybe (PublicKeyHash, Tez)) -> m (Event t (SecretKey, PublicKeyHash))
        accountItem (SecretKey _ sc dp) dynPkhTez = do
          let selected = demuxed selectionDemux $ Just $ SecretKey ledger sc dp
              loaded = isJust <$> dynPkhTez
          (e, _) <- elDynAttr' "div" (ffor loaded $ \l -> "class" =: (if l then "link item" else "item")) $ do
            dyn_ $ ffor dynPkhTez $ \case
              Nothing -> do
                divClass "ui active tiny inline blue loader" blank
                text "Importing PKH..."
              Just (pkh, tz) -> do
                SemUi.ui "div" (def & SemUi.classes .~ SemUi.Dyn (bool "icon-check" "active icon-check" <$> selected)) blank
                text $ toPublicKeyHashText pkh
                fancyTez tz
          let f mpkh () = fmap (\(pkh, _) -> (SecretKey ledger sc dp, pkh)) mpkh
          pure $ attachWithMaybe f (current dynPkhTez) (domEvent Click e)

    accounts <- watchLedgerAccounts $ (: secretKeys) <$> manualSk

    chosen <- divClass "ui block list" $ fmap leftmost $ for secretKeys $ \sk -> accountItem sk $ MMap.lookup sk <$> accounts
    divClass "explanation" $ text "Don't see your account? Enter a specific signing curve and derivation path."

    selection :: Dynamic t (Maybe (SecretKey, PublicKeyHash))
      <- foldDyn (\a b -> if b == Just a then Nothing else Just a) Nothing $ leftmost [chosen, chosen']
    let selectionDemux = demux $ (fmap . fmap) fst selection

    manualSk <- divClass "two fields" $ do
      curve <- divClass "ui field" $ do
        el "label" $ text "Signing Curve"
        SemUi.dropdown (def & SemUi.dropdownConfig_fluid SemUi.|~ True) (Identity $ head curves) never $ SemUi.TaggedStatic $
          Map.fromList $ ffor curves $ \c -> (c, text $ toSigningCurveText c)
      derivation <- divClass "ui field" $ do
        el "label" $ text "Derivation Path"
        let initVal = unDerivationPath primaryDeriv
        dp <- formItem' "required" $ validatedInput validateBIP32 $ def
          & Txt.setInitial initVal
        let mDerivPath = ffor dp $ \case
              Right t -> Just t
              Left _ -> Nothing
        fmap DerivationPath <$> holdDyn initVal (fmapMaybe id $ updated mDerivPath)
      pure $ SecretKey ledger . runIdentity <$> value curve <*> derivation

    specificRequest <- debounce 1 $ updated manualSk
    _ <- requestingIdentity $ public . PublicRequest_ShowLedger <$> specificRequest

    manualD <- holdUniqDyn $ ffor2 manualSk accounts $ \sk as -> (,) sk <$> MMap.lookup sk as
    chosen' <- divClass "ui block list" $ switchHold never <=< dyn $ ffor manualD $ \case
      Just (sk, (pkh, tz)) -> accountItem sk (pure (Just (pkh, tz)))
      Nothing -> divClass "item" $ do
        divClass "ui active tiny inline blue loader" blank
        text "Importing PKH..."
        pure never

    divClass "ui divider" blank
    divClass "ui error message" $ text "Select an address from the list above to continue."
    elAttr "button" ("type" =: "submit" <> "class" =: "ui primary button") $ text "Continue"

    pure selection

  hasError <- holdDyn False $ leftmost
    [ True <$ ffilter isNothing (tag (current selection) submitted)
    , False <$ updated selection
    ]
  let register = fmapMaybe id $ tag (current selection) submitted
  pure register

validateBIP32 :: Validator.Validator t m Text
validateBIP32 = Validator.Validator isValidBIP32 id

-- Proper format [num]'/[num]'
-- eg. "0'/0'"
-- eg. "0'/2147483647'/3424'/23134'"
-- eg. "2147483647'/2147483647'"
isValidBIP32 :: Text -> Either Text Text
isValidBIP32 t
  | T.null t = Right t -- Empty string selects root path
  | otherwise = go (8 :: Int) t -- Can have upto 8 numbers
  where
    go n t1 = parseDigit t1 >>= \case
      "'" -> Right t -- return the original
      t2 -> parseMiddle t2 >>= (\t3 -> if n > 1 then go (n - 1) t3 else errFormat)
    errFormat = Left "Incorrect format"
    parseMiddle t1 = case T.stripPrefix "'/" t1 of
      Nothing -> errFormat
      Just t2 -> Right t2
    maxVal = 2 ^ (31 :: Int) - 1 :: Int
    parseDigit t1 = case T.takeWhile isDigit t1 of
      "" -> errFormat
      dt -> case readMaybe (T.unpack dt) of
        Just v -> if v >= 0 && v <= maxVal
          then Right $ T.dropWhile isDigit t1
          else Left $ "Numerical value should be between 0 and " <> tshow maxVal
        Nothing -> errFormat

setupComplete
  :: MonadRhyoliteFrontendWidget Bake t m
  => (SecretKey, PublicKeyHash)
  -> m (Event t ())
setupComplete (_sk, pkh) = divClass "central" $ do
  elClass "h5" "ui header" $ do
    icon "blue icon-check"
    text "Setup is complete!"
  elClass "h6" "ui header prompt-text" $ text $ "Kiln is now running a baker using the address: " <> toPublicKeyHashText pkh
  divClass "centered explanation" $ text "If this was the first time you have registered this address as a delegate, this baker will not immediately have rights to bake or endorse. It takes at least 6 cycles after registering to receive rights."
  uiButton "primary" "Continue"
