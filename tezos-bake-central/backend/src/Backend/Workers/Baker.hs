{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Backend.Workers.Baker where

import Prelude hiding (cycle)

import Control.Arrow ((&&&))
import Control.Lens (anyOf, set, (<>=), (<<>=), (%=), ix, over, _4, ifoldMap, at, (.=), FoldableWithIndex, (^..))
import Control.Exception (handle, SomeException)
import Control.Concurrent.STM (atomically)
import Control.Monad (guard, mzero)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT(..), MonadError, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLoggerIO, MonadLogger, logDebug, logDebugSH, logErrorSH, LoggingT(..))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (MonadState, execStateT, gets, modify)
import Control.Monad.Logger (logDebug, logDebugSH, logErrorSH)
import Control.Monad.Reader (ReaderT (..), lift)
import Control.Monad.State (execStateT, gets, modify)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap(..))
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup ((<>), Max(..))
import Data.Sequence (Seq())
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Database.Groundhog.Postgresql
import Database.Id.Class
import Database.Id.Groundhog
import Reflex (fforMaybe, fmapMaybe)
import Rhyolite.Backend.DB (MonadBaseNoPureAborts, runDb, selectMap)
import Rhyolite.Backend.DB.PsqlSimple (executeQ, queryQ, In(..))
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.DB.Serializable
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Schema (Json(..))
import Safe (maximumDef, minimumDef)

import Tezos.Types
import qualified Tezos.V010.Types as V010
import qualified Tezos.V005.Types as V005
import Tezos.NodeRPC (accountCrossCompat_delegatePkh, blockCrossCata)

import Backend.Config (AppConfig (..), HasAppConfig)
import Backend.Alerts
import Backend.Common (worker', AppSerializable)
import Backend.Config (AppConfig (..))
import Backend.IndexQueries (levelToCycle, getLatestProtocolConstants)
import Backend.NodeRPC
import Backend.Schema
import Backend.STM (atomicallyWith)
import Backend.Alerts (clearMissedBake, reportMissedBake)
import Common (curryMap)
import Common.Schema
import ExtraPrelude

import Data.Align hiding (zip)
import Data.These (These(..), these)

-- TODO: This only loops through one cycle at a time, per block;  we don't need to wait that long (although it may still end up doing the right thing eventually)

bakerRightsWorker
  :: forall m. MonadIO m
  => NodeDataSource
  -> Int
  -> m (IO ())
bakerRightsWorker nds rightsHistoryWindow = worker' "bakerRightsWorker" $ (<* waitForNewHead nds) $ runLoggingEnv (_nodeDataSource_logger nds) $ do
  res :: Either CacheError () <- flip runReaderT nds $ runExceptT $ do
    (latestBranchInfo, protocolConstants) <- runNodeQueryT getLatestProtocolConstants

    $(logDebug) "Update baker cycle."
    let
      db = _nodeDataSource_pool nds
      chainId = _nodeDataSource_chain nds
      headHash :: BlockHash = latestBranchInfo ^. hash
      headLevel = latestBranchInfo ^. level
      endOfCycle = headLevel - latestBranchInfo ^. branchInfo_cyclePosition +
        protocolConstants ^. protoInfo_blocksPerCycle - 1

    --  * compute the list of rights we "want" to have and the list we actually have; their difference is the rights we need
    --  * then actually obtain the rights for all bakers at the oldest cycle we still want.
    needProgress :: MonoidalMap PublicKeyHash (Max BakerRightsProgress) <- lift @(ExceptT CacheError) $ runDb (Identity db) $ do
      bakerPKHs :: [PublicKeyHash] <- project Baker_publicKeyHashField (Baker_dataField ~> DeletableRow_deletedSelector ==. False)
      let
        inBakerPKHs = In bakerPKHs

      -- hot table, rewrite using psql-simple to avoid ==.
      bakerRightsProgress' :: Map (Id BakerRightsProgress) BakerRightsProgress <-
        [queryQ|
          SELECT "id", "chainId", "publicKeyHash", "progress"
          FROM "BakerRightsProgress"
          WHERE "publicKeyHash" in ?inBakerPKHs
            AND "chainId" = ?chainId
        |] <&> Map.fromList . fmap (\(i, ch, p, pr) -> (i, BakerRightsProgress ch p pr))

      -- here's what we've got:
      let
        haveProgress :: MonoidalMap PublicKeyHash (Max BakerRightsProgress)
        haveProgress = flip foldMap bakerRightsProgress' $
          \p -> MMap.singleton (_bakerRightsProgress_publicKeyHash p) (Max p)

      -- this is all of the progress we could possibly want.
      -- we indicate that the progress we've made is none by setting the progress to 'head_level - rightsHistoryWindow - 1'
      return $ (haveProgress <>) $ MMap.fromList $ do
        pkh <- bakerPKHs
        let
          v = BakerRightsProgress
            { _bakerRightsProgress_chainId = chainId
            , _bakerRightsProgress_publicKeyHash = pkh
            , _bakerRightsProgress_progress = headLevel - fromIntegral rightsHistoryWindow - 1
            }
        return (pkh, Max v)

    let
      pkhs :: Set PublicKeyHash
      pkhs = Set.fromList $ fmap (_bakerRightsProgress_publicKeyHash . getMax) $ toList needProgress
      -- drop the already completed bakers.
      mUnfinished :: Maybe (NonEmpty BakerRightsProgress)
      mUnfinished = nonEmpty $ fold $ flip MMap.map needProgress $ \(Max p) -> do
        guard (_bakerRightsProgress_progress p <= endOfCycle)
        return p

      toChunks :: Int -> [a] -> [[a]]
      toChunks _ [] = []
      toChunks chunkSize l = case splitAt chunkSize l of
        (chunk, rest) -> chunk : toChunks chunkSize rest


    $(logDebugSH) ("Baker rights TODO:" :: Text, mUnfinished)
    for_ mUnfinished $ \(aBakerRight :| _) -> do
      let bakerMinBound = _bakerRightsProgress_progress aBakerRight + 1
          bakerMaxBound = endOfCycle
          lvlChunks = toChunks 50 [bakerMinBound .. bakerMaxBound]
      for_ lvlChunks $ \lvlChunk -> do
        let lvls = Set.fromList lvlChunk
            maxLvl = Set.findMax lvls
        -- At this point, our use of the earlier queried BakerRightsCycleProgress is "useless",  we've previously made at least that much progress, so it tells us which we should work on,
        (reqBakers, reqEndorsers) <- runNodeQueryT $ liftA2 (,)
          (nodeQueryIx $ NodeQueryIx_BakingRights headHash lvls)
          (nodeQueryIx $ NodeQueryIx_EndorsingRights headHash lvls)
        let
          pri1bakers :: [BakingRights]
          pri1bakers = filter (\br -> (flip Set.member pkhs . _bakingRights_delegate) br && ((== 0) . _bakingRights_priority) br) $ toList reqBakers
          endorsers :: [EndorsingRights]
          endorsers = filter (flip Set.member pkhs . _endorsingRights_delegate) $ toList reqEndorsers

          bakerRightCycleInfo :: PublicKeyHash -> BakerRightsProgress
          bakerRightCycleInfo pkh = BakerRightsProgress
            { _bakerRightsProgress_chainId = chainId
            , _bakerRightsProgress_publicKeyHash = pkh
            , _bakerRightsProgress_progress = maxLvl
            }
          bakerRights :: Maybe (Id BakerRightsProgress) -> PublicKeyHash -> [BakerRight]
          bakerRights pid pkh = flip (maybe mempty) pid $ \pid' ->
            map (\br -> BakerRight
              { _bakerRight_branch = pid'
              , _bakerRight_level = _bakingRights_level br
              , _bakerRight_right = RightKind_Baking
              , _bakerRight_slots = Nothing
              }) (filter ((== pkh) ._bakingRights_delegate) pri1bakers) ++
            map (\end -> BakerRight
              { _bakerRight_branch = pid'
              , _bakerRight_level = _endorsingRights_level end
              , _bakerRight_right = RightKind_Endorsing
              , _bakerRight_slots = Just $ length $ _endorsingRights_slots end
              }) (filter ((== pkh) ._endorsingRights_delegate) endorsers)

        $(logDebug) ("bakerrights working lvl:" <> tshow (unRawLevel $ Set.findMax lvls))
        lift @(ExceptT CacheError) $ runDb (Identity db) $ for_ pkhs $ \pkh -> do
          let
            newProgress = bakerRightCycleInfo pkh
          progress' :: [(Id BakerRightsProgress, BakerRightsProgress)] <- Map.toList <$> selectMap BakerRightsProgressConstructor
            ( BakerRightsProgress_publicKeyHashField `in_` [pkh]
              &&. BakerRightsProgress_chainIdField `in_` [chainId]
            )
          progressId :: Maybe (Id BakerRightsProgress) <- case nonEmpty progress' of
            Nothing -> Just . toId <$> insert newProgress -- assert lvl == _rightsCycleInfo_minLevel
            Just ((pId, p):|_)
              --  | _bakerRightsCycleProgress_progress < lvl-1 -> TODO sulk
              | _bakerRightsProgress_progress p < maxLvl -> do
                _ <- [executeQ|
                  UPDATE "BakerRightsProgress"
                  SET progress = ?maxLvl
                  WHERE "id" = ?pId
                  |]

                return $ Just pId
              | otherwise -> return Nothing -- already have this progress, do nothing.
          rights <- for (bakerRights progressId pkh) $ \r -> insert r $> r
          let
            maybeNotify :: forall m' . PersistBackend m' => Id BakerRightsProgress -> BakerRightsProgress -> [BakerRight] -> m' ()
            maybeNotify x y z = when (_bakerRightsProgress_progress y == bakerMaxBound) $
              notify NotifyTag_BakerRightsProgress (x,y,z)
            {-# INLINE maybeNotify #-}
          sequence_ $ maybeNotify <$> progressId <*> pure newProgress <*> pure rights

      -- Trim old rights from the database
    let oldestLevel = headLevel - fromIntegral rightsHistoryWindow
    void $ runDb (Identity db) [executeQ|
      DELETE FROM "CacheBakingRights" WHERE "level" < ?oldestLevel;
      DELETE FROM "CacheEndorsingRights" WHERE "level" < ?oldestLevel;
      DELETE FROM "BakerRight" WHERE "level" < ?oldestLevel;
      |]

  case res of
    Right _ -> pure ()
    Left err -> logCacheError "bakerRightsWorker" err

  $(logDebug) $ "BAKERRIGHTSWORKER STEP" <> tshow res

bakerWorker
  :: forall m. MonadIO m
  => AppConfig
  -> NodeDataSource
  -> m (IO ())
bakerWorker appConfig nds = worker' "bakerWorker" $ (<* waitForNewHead nds) $ runLoggingEnv (_nodeDataSource_logger nds) $ do
  let db = _nodeDataSource_pool nds

  res <- flip runReaderT nds $ runExceptT $ do
    (bakerInt, protoInfo, headCycle, headBlock, currentState :: [(Baker, Maybe BakerDetails)]) <- runNodeQueryT $ do
      (headBlock, protoInfo) <- getLatestProtocolConstants
      headCycle <- levelToCycle (headBlock ^. hash, headBlock ^. level) (headBlock ^. level)
      bakers :: Map PublicKeyHash Baker <- Map.fromList <$> project (Baker_publicKeyHashField, BakerConstructor) (Baker_dataField ~> DeletableRow_deletedSelector ==. False)
      bakerInt :: Maybe PublicKeyHash <- join . listToMaybe <$> project (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector ~> BakerDaemonInternalData_publicKeyHashSelector)
          (BakerDaemonInternal_dataField ~> DeletableRow_deletedSelector ==. False)
      details :: Map PublicKeyHash BakerDetails <- Map.fromList <$> project
        (BakerDetails_publicKeyHashField, BakerDetailsConstructor)
        (BakerDetails_publicKeyHashField `in_` Map.keys bakers)
      return $ (bakerInt, protoInfo, headCycle, headBlock,) $ catMaybes $ toList $
        alignWith (these (Just . ($ Nothing) . (,)) (const Nothing) (curry (Just . fmap Just))) bakers details

    wantedActions <- for currentState $ \(baker, details) -> do
      let isInternal = Just (_baker_publicKeyHash baker) == bakerInt
      res <- (Right <$> getWantedAction protoInfo headBlock headCycle baker details isInternal)
        `catchError` (pure . Left)
      case res of
        Right commit -> do
          $(logDebug) $ "bakerWorker DONE with baker: " <> tshow baker
          pure $ Just commit
        Left (err :: CacheError) -> do
          $(logErrorSH) ("bakerWorker failed to process baker: " <> tshow baker, err)
          pure Nothing

    -- beware of the jellyfish
    lift @(ExceptT CacheError) $ runDb (Identity db) $ runReaderT (sequence_ $ fmapMaybe id wantedActions) appConfig

  case res of
    Right () -> $(logDebug) "bakerWorker DONE"
    Left (err :: CacheError) -> logCacheError "bakerWorker" err


-- separating the monad that can do RPC(mPrepare) from the one that can do
-- SQL(mCommit) makes it a little easier to set up the transactions to succeed.
--
-- Make sure that the mCommit action has enough information to bail out or do
-- nothing if the data gathered in mPrepare can be stale
{-# INLINE getWantedAction #-}
getWantedAction
  :: forall mPrepare rP blk.
  ( BlockLike blk
  , MonadIO mPrepare, MonadReader rP mPrepare, HasNodeDataSource rP, MonadLogger mPrepare
  , MonadBaseNoPureAborts IO mPrepare, MonadMask mPrepare, MonadLoggerIO mPrepare
  )
  => ProtoInfo -> blk -> Cycle -> Baker -> Maybe BakerDetails -> Bool -> ExceptT CacheError mPrepare (AppSerializable ())
getWantedAction protoInfo headBlock headCycle baker details isInternal = do
  let
    headHash = headBlock ^. hash
    headPred = headBlock ^. predecessor
    headLvl = headBlock ^. level
    pkh = _baker_publicKeyHash baker
    headFitness = headBlock ^. fitness
    -- for each baker; follow the branch it was on previously to the new head
    -- check at each level to see if the baker had rights there;
    -- if so, examine the block to see if they exercized those rights
    -- if not, report an error; if so, clear an error.
    detailsBranch :: BlockHash = maybe headPred (view hash . _bakerDetails_branch) details
  detailsBlock <- nodeQueryDataSource $ NodeQuery_Block detailsBranch
  bakingEndorsingAlerts :: [AppSerializable ()] <- for [headLvl .. detailsBlock ^. level] $ \lvl -> do
    thisBlock <- nodeQueryDataSource $ NodeQuery_BlockPred headHash (headLvl - lvl)
    predBlock <- nodeQueryDataSource $ NodeQuery_BlockPred headHash (headLvl - lvl + 1)
    bakingRights :: Seq BakingRights <- runNodeQueryT $ nodeQueryIx $ NodeQueryIx_BakingRights headHash (Set.singleton lvl)
    bakingAlerts :: [AppSerializable ()]
                 <- whenM (any (\br -> ((== 0) . _bakingRights_priority) br && ((== _baker_publicKeyHash baker) . _bakingRights_delegate) br) bakingRights) $ do
      let action =
            bool (reportMissedBake (thisBlock ^. timestamp)) clearMissedBake ((thisBlock ^. blockMetadata . blockMetadata_baker) == _baker_publicKeyHash baker)
              (headBlock ^. fitness)
              RightKind_Baking
              (baker ^. baker_publicKeyHash)
              lvl
      return $ pure action

    -- endorsements *on* this block are *of* the previous block
    endorsers :: Seq EndorsingRights <- runNodeQueryT $ nodeQueryIx $ NodeQueryIx_EndorsingRights headHash (Set.singleton $ lvl - 1)
    endorsingAlerts :: [AppSerializable ()]
                    <- whenM (elem (_baker_publicKeyHash baker) $ _endorsingRights_delegate <$> endorsers) $ do

      let
        endorserDelegates = blockCrossCata
          (^..V010.block_operations . traverse . traverse . V010.operation_contents . traverse . V010._OperationContents_EndorsementWithSlot . V010.operationContentsEndorsementWithSlot_metadata . V010.endorsementMetadata_delegate)
          (^..V005.block_operations . traverse . traverse . V005.operation_contents . traverse . V005._OperationContents_Endorsement . V005.operationContentsEndorsement_metadata . V005.endorsementMetadata_delegate)
          thisBlock
        mkAction = bool (reportMissedBake (predBlock ^. timestamp)) clearMissedBake (_baker_publicKeyHash baker `elem` endorserDelegates)
        action = mkAction (headBlock ^. fitness) RightKind_Endorsing (baker ^. baker_publicKeyHash) (lvl - 1)
      return $ pure action

    return $ sequence_ $ bakingAlerts <> endorsingAlerts

  -- TODO divide above and below this into two separate workers.

  -- This is the things that fails. First go look up the account, and see who/if
  -- it is delegated (`NodeQuery_Account`). Only proceed if there is a delegate,
  -- and cache that.
  delegate <- (^.accountCrossCompat_delegatePkh) <$>
    nodeQueryDataSource (NodeQuery_Account headHash (Implicit pkh))
  selfDelegateActions <- case delegate of
    Nothing -> pure []
    Just delegatePkh -> do
      di <- nodeQueryDataSource (NodeQuery_DelegateInfo headHash headLvl delegatePkh)
      let
        gracePeriod = _cacheDelegateInfo_gracePeriod di

        updateDetails :: AppSerializable ()
        updateDetails = do
          existingIds <- project BakerDetails_publicKeyHashField
            ( BakerDetails_publicKeyHashField ==. delegatePkh
            &&. BakerDetails_branchField ~> VeryBlockLike_fitnessSelector <=. headFitness
            )

          let
            newVal = BakerDetails
              { _bakerDetails_publicKeyHash = delegatePkh
              -- , _bakerDetails_nextBakeRights = _bakingRights_level <$> Map.lookup delegatePkh bakingRights
              -- , _bakerDetails_nextEndorseRights = _endorsingRights_level <$> Map.lookup delegatePkh endorsingRights
              , _bakerDetails_branch = mkVeryBlockLike headBlock
              , _bakerDetails_delegateInfo = Just $ Json di
              }
          case nonEmpty existingIds of
            Nothing -> void $ insert newVal
            Just brids -> for_ brids $ \brid ->
              update
                [ BakerDetails_branchField =. _bakerDetails_branch newVal
                , BakerDetails_delegateInfoField =. _bakerDetails_delegateInfo newVal
                ]
                ( BakerDetails_publicKeyHashField ==. brid)
          notifyDefault newVal

        -- Within a single run of a kiln instance, the fitness of blocks we observe is non-decreasing,
        -- but there might be multiple instances or resets, so we can only clear an error when a fitter block claims it's gone.
        deactivationAlerts :: AppSerializable ()
        deactivationAlerts =
          if _cacheDelegateInfo_deactivated di
            then do
              clearBakerDeactivationRisk delegatePkh headFitness
              reportBakerDeactivated delegatePkh protoInfo headFitness
            else do
              clearBakerDeactivated delegatePkh headFitness
              if 1 >= gracePeriod - headCycle
                then reportBakerDeactivationRisk delegatePkh gracePeriod headCycle protoInfo headFitness
                else clearBakerDeactivationRisk delegatePkh headFitness

        isInsufficientFunds = _cacheDelegateInfo_stakingBalance di < _protoInfo_tokensPerRoll protoInfo

        insufficientFundAlerts :: AppSerializable ()
        insufficientFundAlerts = bool clearInsufficientFunds reportInsufficientFunds isInsufficientFunds baker

        -- updateBakerDataInternal :: mCommit ()
        updateBakerDataInternal = update
          [BakerDaemonInternal_dataField ~> DeletableRow_dataSelector ~>
           BakerDaemonInternalData_insufficientFundsSelector =. isInsufficientFunds]
          CondEmpty

      pure $ [deactivationAlerts, updateDetails] ++ if isInternal
        then [insufficientFundAlerts, updateBakerDataInternal]
        else []

  return $ sequence_ $ selfDelegateActions ++ bakingEndorsingAlerts
