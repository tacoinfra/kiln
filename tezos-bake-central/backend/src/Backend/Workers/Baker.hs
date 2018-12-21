{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Workers.Baker where

import Control.Concurrent.STM (atomically)
import Control.Monad (mzero)
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (logDebug, logDebugSH, logErrorSH)
import Control.Monad.Reader (ReaderT (..), lift)
import Control.Monad.State (execStateT, gets, modify)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.List.NonEmpty (nonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Sequence (Seq())
import qualified Data.Set as Set
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB (runDb, selectMap)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Schema (Id (..), Json(..))

import Tezos.Types

import Backend.Alerts
import Backend.CachedNodeRPC
import Backend.Common (worker')
import Backend.Config (AppConfig (..))
import Backend.Schema
import Backend.STM (atomicallyWith)
import Common.Schema
import ExtraPrelude

bakerWorker
  :: forall m. MonadIO m
  => NodeDataSource
  -> AppConfig
  -> m (IO ())
bakerWorker nds appConfig = worker' $ (<* waitForNewHead nds) $ runLoggingEnv (_nodeDataSource_logger nds) $ do
  (protoInfo, headM) <- liftIO $ atomically $
    liftA2 (,) (waitForParams nds) (dataSourceHead nds)

  let db = _nodeDataSource_pool nds
  res <- runExceptT $ for_ headM $ \headBlock -> flip runReaderT nds $ do
    $(logDebug) "Update baker cycle."
    let
      headHash :: BlockHash = headBlock ^. hash
      headLevel :: RawLevel = headBlock ^. level
      maxLevel = maxRightsLevel protoInfo headLevel
      latestCycle = Cycle $ unRawLevel $ headLevel `div` fromIntegral (_protoInfo_blocksPerCycle protoInfo)
    $(logDebug) $ "Head level is " <> tshow (unRawLevel headLevel) <> " in cycle " <> tshow (unCycle latestCycle)

    -- get the exist configured bakers from the DB, and any info about them we've retrieved previously
    bakers :: Map (Id Baker) (Baker, Maybe BakerDetails) <- runDb (Identity db) $ do
      bakers :: Map (Id Baker) Baker <- selectMap BakerConstructor (Baker_deletedField ==. False)
      bakerDetails0 :: Map (Id BakerDetails) BakerDetails <- selectMap BakerDetailsConstructor
        (BakerDetails_publicKeyHashField
        `in_` toList (fmap _baker_publicKeyHash bakers))

      let bakerDetails = Map.fromList $ fmap (\x -> (_bakerDetails_publicKeyHash x, x)) $ toList bakerDetails0
      return $ flip fmap bakers $ \b -> (b, Map.lookup (_baker_publicKeyHash b) bakerDetails)

    -- decide which blocks to operate on based on what we've done with the baker already
    bakers1 :: Map (Id Baker) (Baker, Maybe BakerDetails, [BlockHash]) <- for bakers $ \(baker, maybeBakerDetails) -> do
      newBHs <- runMaybeT $ do
        bakerDetails <- MaybeT $ pure maybeBakerDetails
        (_invalidatedBlockHashes, blockHashes) <- MaybeT $ atomicallyWith $
          enumerateBranches (_bakerDetails_branch bakerDetails) headHash
        pure blockHashes
      pure (baker, maybeBakerDetails, maybe [headHash] id newBHs)

    let
      bakersSet = Set.fromList $ _baker_publicKeyHash . fst <$> Map.elems bakers

      mkFillMap
        :: forall m' s e a. (MonadIO m', MonadReader s m', HasNodeDataSource s, MonadError e m', AsCacheError e)
        => (BlockHash -> RawLevel -> NodeQuery (Seq a)) -> (a -> PublicKeyHash) -> m' (Map PublicKeyHash a)
      mkFillMap cacheQ getter = flip execStateT Map.empty $ runMaybeT $ do
        let queries = flip fmap [headLevel..maxLevel] $ \lvl ->
              nodeQueryDataSource $ cacheQ headHash lvl
        for_ queries $ \query -> do
          done <- gets $ (bakersSet `Set.isSubsetOf`) . Map.keysSet
          when done mzero
          stuff <- query
          for_ stuff $ \thing -> do
            modify $ Map.insertWith (\_new old -> old) (getter thing) thing
      {-# INLINE mkFillMap #-}

    bakingRights :: Map PublicKeyHash BakingRights <- mkFillMap NodeQuery_BakingRights _bakingRights_delegate
    endorsingRights :: Map PublicKeyHash EndorsingRights <- mkFillMap NodeQuery_EndorsingRights _endorsingRights_delegate

    runDb (Identity (_nodeDataSource_pool nds)) $ ifor_ bakers $ \bid (baker, _bakerDetails) -> do
      let pkh = _baker_publicKeyHash baker
      $(logDebug) $ "Updating rights data baker " <> toPublicKeyHashText pkh
      existingIds :: [Id BakerDetails] <- fmap toId <$> project AutoKeyField (BakerDetails_publicKeyHashField ==. pkh)
      di <- lift $ nodeQueryDataSource $ NodeQuery_DelegateInfo headHash headLevel pkh
      let
        newVal = BakerDetails
          { _bakerDetails_publicKeyHash = pkh
          , _bakerDetails_nextBakeRights = _bakingRights_level <$> Map.lookup pkh bakingRights
          , _bakerDetails_nextEndorseRights = _endorsingRights_level <$> Map.lookup pkh endorsingRights
          , _bakerDetails_branch = headBlock ^. hash
          , _bakerDetails_delegateInfo = Just $ Json di
          }
      case nonEmpty existingIds of
        Nothing -> void $ insert newVal
        Just brids -> for_ brids $ \brid -> updateId brid
          [ BakerDetails_nextBakeRightsField =. _bakerDetails_nextBakeRights newVal
          , BakerDetails_nextEndorseRightsField =. _bakerDetails_nextEndorseRights newVal
          , BakerDetails_branchField =. _bakerDetails_branch newVal
          , BakerDetails_delegateInfoField =. _bakerDetails_delegateInfo newVal
          ]
      notify $ mkDefaultNotify newVal

      let gracePeriod = _cacheDelegateInfo_gracePeriod di
          fit = headBlock ^. fitness
      flip runReaderT appConfig $ do
        if _cacheDelegateInfo_deactivated di
          then reportBakerDeactivated bid protoInfo fit
          else do
            clearBakerDeactivated bid fit
            if (1 >= gracePeriod - latestCycle)
              then reportBakerDeactivationRisk bid gracePeriod latestCycle protoInfo fit
              else clearBakerDeactivationRisk bid fit

  case res of
    Right _ -> pure ()
    Left (err :: CacheError) -> $(logErrorSH) ("bakerWorker" :: String, err)
