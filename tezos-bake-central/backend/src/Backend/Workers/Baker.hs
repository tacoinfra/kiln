{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

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
import Control.Monad.Reader (ReaderT (..), lift)
import Control.Monad.State (execStateT, gets, modify)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap(..))
import qualified Data.Map.Monoidal as MMap
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>), Max(..))
import Data.Sequence (Seq())
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Time (UTCTime)
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
import qualified Tezos.Genesis.Block as Genesis
import qualified Tezos.V013.Types as V013
import Tezos.CrossCompat.Account
import Tezos.CrossCompat.Block
import Tezos.Unsafe (unsafeEstimatePastTimestamp)

import Backend.Config (AppConfig (..), HasAppConfig, askAppConfig)
import Backend.Alerts
import Backend.Common (worker', AppSerializable)
import Backend.Config (AppConfig (..))
import Backend.IndexQueries (endOfPreservedCycles, levelToCycle, getLatestProtocolConstants)
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
bakerRightsWorker nds rightsHistoryWindow = worker' "bakerRightsWorker" $ (<* waitForNewFinalHead nds) $ runLoggingEnv (_nodeDataSource_logger nds) $ do
  res :: Either KilnRpcError () <- flip runReaderT nds $ runExceptT $ do
    (latestBranchInfo, protocolConstants) <- runNodeQueryT getLatestProtocolConstants

    $(logDebug) "Update baker cycle."
    let
      db = _nodeDataSource_pool nds
      chainId = _nodeDataSource_chain nds
      headHash :: BlockHash = latestBranchInfo ^. hash
      headLevel = latestBranchInfo ^. level
      endOfPreservedCyclesLvl = endOfPreservedCycles latestBranchInfo protocolConstants

    --  * compute the list of rights we "want" to have and the list we actually have; their difference is the rights we need
    --  * then actually obtain the rights for all bakers at the oldest cycle we still want.
    needProgress :: MonoidalMap PublicKeyHash (Max BakerRightsProgress) <- lift @(ExceptT KilnRpcError) $ runDb (Identity db) $ do
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
        guard (_bakerRightsProgress_progress p <= endOfPreservedCyclesLvl)
        return p

      toChunks :: Int -> [a] -> [[a]]
      toChunks _ [] = []
      toChunks chunkSize l = case splitAt chunkSize l of
        (chunk, rest) -> chunk : toChunks chunkSize rest


    $(logDebugSH) ("Baker rights TODO:" :: Text, mUnfinished)
    for_ mUnfinished $ \(aBakerRight :| _) -> do
      let bakerMinBound = _bakerRightsProgress_progress aBakerRight + 1
          bakerMaxBound = endOfPreservedCyclesLvl
          lvlChunks = toChunks 50 [bakerMinBound .. bakerMaxBound]
      for_ lvlChunks $ \lvlChunk -> do
        let lvls = Set.fromList lvlChunk
            maxLvl = Set.findMax lvls
        -- At this point, our use of the earlier queried BakerRightsCycleProgress is "useless",  we've previously made at least that much progress, so it tells us which we should work on,
        (reqBakers, reqEndorsers) <- runNodeQueryT $ liftA2 (,)
          (nodeQueryIx $ NodeQueryIx_BakingRights headHash lvls)
          (nodeQueryIx $ NodeQueryIx_EndorsingRights headHash lvls)
        let
          pri1bakers :: [BakingRightsCrossCompat]
          pri1bakers = filter (\br -> (flip Set.member pkhs . view bakingRightsCrossCompat_delegate) br) $ toList reqBakers
          endorsers :: [EndorsingRightsCrossCompat]
          endorsers = filter (any (flip Set.member pkhs) . view endorsingRightsCrossCompat_delegates) $ toList reqEndorsers

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
              , _bakerRight_level = br ^. bakingRightsCrossCompat_level
              , _bakerRight_right = RightKind_Baking
              }) (filter ((== pkh) . view bakingRightsCrossCompat_delegate) pri1bakers) ++
            mapMaybe (\end ->
              if pkh `elem` end ^. endorsingRightsCrossCompat_delegates
                then Just $ BakerRight
                  { _bakerRight_branch = pid'
                  , _bakerRight_level = end ^. endorsingRightsCrossCompat_level
                  , _bakerRight_right = RightKind_Endorsing
                  }
                else Nothing
              ) endorsers

        $(logDebug) ("bakerrights working lvl:" <> tshow (unRawLevel $ Set.findMax lvls))
        lift @(ExceptT KilnRpcError) $ runDb (Identity db) $ for_ pkhs $ \pkh -> do
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
    Left err -> logKilnRpcError "bakerRightsWorker" err

  $(logDebug) $ "BAKERRIGHTSWORKER STEP" <> tshow res

bakerWorker
  :: forall m. MonadIO m
  => AppConfig
  -> NodeDataSource
  -> Int
  -> m (IO ())
bakerWorker appConfig nds rightsHistoryWindow = worker' "bakerWorker" $ (<* waitForNewFinalHead nds) $ runLoggingEnv (_nodeDataSource_logger nds) $ do
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

    for_ currentState $ \(baker, details) -> do
      let isInternal = Just (_baker_publicKeyHash baker) == bakerInt
          headLvl = headBlock ^. level
          -- For each baker check at most 'rightsHistoryWindow' from the current head down
          -- to 'bakerDetails_branch' level;
          -- check at each level to see if the baker had rights there;
          -- if so, examine the block to see if they exercized those rights
          -- if not, report an error; if so, clear an error.
          cutoffLevel :: RawLevel = max (headLvl - fromIntegral rightsHistoryWindow) $
            maybe (headLvl - 1) (view level . _bakerDetails_branch) details
      for_ [headLvl, headLvl - 1 .. cutoffLevel + 1] $ \lvl -> do
        checkRes <- (Right <$> checkMissedOpportunities nds appConfig protoInfo headBlock baker isInternal lvl) `catchError` (pure . Left)
        case checkRes of
          Right transaction -> runDb (Identity db) $ runReaderT transaction appConfig
          Left err -> $(logErrorSH) $ "bakerWorker failed to check opportunities on level " <> tshow lvl <> " :" <> tshow err

      updateRes <- (Right <$> updateDelegateDetails protoInfo headBlock headCycle baker details isInternal) `catchError` (pure . Left)
      case updateRes of
        Right transaction -> do
          runDb (Identity db) $ runReaderT transaction appConfig
          $(logDebug) $ "bakerWorker DONE with baker: " <> tshow baker
        Left err ->
          $(logErrorSH) $ "bakerWorker failed to update baker details for " <> tshow baker <> " :" <> tshow err

  case res of
    Right () -> $(logDebug) "bakerWorker DONE"
    Left (err :: KilnRpcError) -> logKilnRpcError "bakerWorker" err

checkMissedOpportunities
  :: ( BlockLike blk
     , MonadIO m, MonadReader rP m, HasNodeDataSource rP, MonadLogger m
     , MonadBaseNoPureAborts IO m, MonadMask m, MonadLoggerIO m)
  => NodeDataSource -> AppConfig -> ProtoInfo -> blk -> Baker -> Bool -> RawLevel -> ExceptT KilnRpcError m (AppSerializable ())
checkMissedOpportunities nds appConfig protoInfo headBlock baker isInternal lvl =  do
  let
    headHash = headBlock ^. hash
    headLvl = headBlock ^. level
    pkh = _baker_publicKeyHash baker
    db = _nodeDataSource_pool nds
    logger = _nodeDataSource_logger nds

    -- TODO use different counters for missed bakes and endorsements instead of weighted one
    incMissedRightsInRow :: Int -> PublicKeyHash -> ReaderT AppConfig Serializable ()
    incMissedRightsInRow weight pkh' = do
      bakerDetails :: [BakerDetails] <- select $ BakerDetails_publicKeyHashField ==. pkh'
      case bakerDetails of
        [] -> pure ()
        bd : _ -> do
          let curMissedInRow = _bakerDetails_missedRightsInRow bd
              newVal = bd { _bakerDetails_missedRightsInRow = curMissedInRow + weight }
          update
            [BakerDetails_missedRightsInRowField =. curMissedInRow + weight] $
            BakerDetails_publicKeyHashField ==. pkh'
          notifyDefault newVal

    clearMissedRightsInRow :: PublicKeyHash -> ReaderT AppConfig Serializable ()
    clearMissedRightsInRow pkh' = do
      bakerDetails :: [BakerDetails] <- select $ BakerDetails_publicKeyHashField ==. pkh'
      case bakerDetails of
        [] -> pure ()
        bd : _ -> do
          let newVal = bd { _bakerDetails_missedRightsInRow = 0 }
          update
            [BakerDetails_missedRightsInRowField =. (0 :: Int)] $
            BakerDetails_publicKeyHashField ==. pkh'
          notifyDefault newVal
    cleanAction = \blockTimestamp blockFitness kind bakerPkh blockLevel -> do
      clearMissedBake blockFitness kind bakerPkh blockLevel
      -- If the internal baker successfully endorses or baker, then there is an
      -- evidence that the Ledger device is connected properly
      when isInternal $ do
        chainId <- _appConfig_chainId <$> askAppConfig
        mbLedgerDisconnectionError :: Maybe (Id ErrorLog) <- listToMaybe . fmap fromOnly <$> [queryQ|
          SELECT el.id
            FROM "ErrorLog" el
            JOIN "ErrorLogBakerLedgerDisconnected" t ON t.log = el.id
           WHERE el.stopped IS NULL
             AND el."chainId" = ?chainId
             AND t."baker#publicKeyHash" = ?pkh
             AND el."lastSeen" < ?blockTimestamp
           ORDER BY el."lastSeen" DESC, el.started DESC
           LIMIT 1
          |]
        whenJust mbLedgerDisconnectionError $ \_ -> clearBakerLedgerDisconnected bakerPkh

    resetHWMAction :: Bool -> ReaderT AppConfig Serializable ()
    resetHWMAction = bool (pure ()) (runLoggingEnv logger $ clearLedgerNeedToResetHWM db appConfig)

  -- Check baking opportunities for the current block and endorsing opprotunities for the
  -- previous block
  thisBlock <- nodeQueryDataSource $ NodeQuery_BlockPred headHash (headLvl - lvl)
  bakingRights :: Seq BakingRightsCrossCompat <- runNodeQueryT $ nodeQueryIx $ NodeQueryIx_BakingRights headHash (Set.singleton lvl)
  bakingAlerts :: [AppSerializable ()]
               <- whenM (any (\br -> ((== _baker_publicKeyHash baker) . view bakingRightsCrossCompat_delegate) br) bakingRights) $ do
    let
      successfulBakeCondition = (thisBlock ^. blockMetadata . blockMetadata_baker) == _baker_publicKeyHash baker
      -- Report missed bake alert if baker missed a bake, or clear the alert otherwise
      missedBakeAction =
          bool reportMissedBake cleanAction successfulBakeCondition
            (thisBlock ^. timestamp)
            (headBlock ^. fitness)
            RightKind_Baking
            (baker ^. baker_publicKeyHash)
            lvl
      -- Increment 'missedAlertsInRow' counter in db if baker missed an endorsement, or set it to zero otherwise
      missedRightsInRowAction = bool (incMissedRightsInRow 5) clearMissedRightsInRow successfulBakeCondition (baker ^. baker_publicKeyHash)
      -- Successful bake is an evidence that high-watermark value is valid, so we clear that alert.
      needToResetHWMAction = resetHWMAction successfulBakeCondition
    return $ pure $ sequence_
      [ missedBakeAction
      , missedRightsInRowAction
      , needToResetHWMAction
      ]
  -- endorsements *on* this block are *of* the previous block
  endorsers :: Seq EndorsingRightsCrossCompat <- runNodeQueryT $ nodeQueryIx $ NodeQueryIx_EndorsingRights headHash (Set.singleton $ lvl - 1)
  endorsingAlerts :: [AppSerializable ()]
                    <- whenM (any (elem (_baker_publicKeyHash baker)) $ view endorsingRightsCrossCompat_delegates <$> endorsers) $ do
      let
        blockBaker = thisBlock ^. blockMetadata . blockMetadata_baker
        mbBlockProposer = thisBlock ^. blockMetadata . blockMetadata_proposer
        endorserDelegates = blockCrossData
            (^..Genesis.block_operations . traverse . traverse . V013.operation_contents . traverse . V013._OperationContents_Endorsement . V013.operationContentsEndorsement_metadata . V013.endorsementMetadata_delegate)
            (^..V013.block_operations . traverse . traverse . V013.operation_contents . traverse . V013._OperationContents_Endorsement . V013.operationContentsEndorsement_metadata . V013.endorsementMetadata_delegate)
            thisBlock
        successfulEndorsementCondition = _baker_publicKeyHash baker `elem` endorserDelegates

        -- Report missed endorsement alert if baker missed an endorsement, or clear the alert otherwise
        missedEndorsementAction = bool reportMissedBake cleanAction successfulEndorsementCondition
          (unsafeEstimatePastTimestamp protoInfo (thisBlock ^. level - 1) thisBlock)
          (headBlock ^. fitness)
          RightKind_Endorsing
          (baker ^. baker_publicKeyHash)
          (lvl - 1)

        -- Report missed endorsement bonus alerts if block proposer is not equal to block baker
        missedBonusAction = whenJust mbBlockProposer $ \blockProposer ->
          when (blockProposer /= blockBaker && blockProposer == _baker_publicKeyHash baker) $
            reportMissedEndorsementBonus (thisBlock ^. timestamp) (baker ^. baker_publicKeyHash) lvl

        -- Increment 'missedAlertsInRow' counter in db if baker missed an endorsement, or set it to zero otherwise
        missedRightsInRowAction = bool (incMissedRightsInRow 1) clearMissedRightsInRow successfulEndorsementCondition (_baker_publicKeyHash baker)

        -- Successful endorsement is an evidence that high-watermark value is valid, so we clear that alert.
        needToResetHWMAction = resetHWMAction successfulEndorsementCondition

      return $ pure $ sequence_
        [ missedEndorsementAction
        , missedBonusAction
        , missedRightsInRowAction
        , needToResetHWMAction
        ]

  return $ sequence_ $ bakingAlerts <> endorsingAlerts

updateDelegateDetails
  :: forall m rP blk.
  ( BlockLike blk
  , MonadIO m, MonadReader rP m, HasNodeDataSource rP, MonadLogger m
  , MonadBaseNoPureAborts IO m, MonadMask m, MonadLoggerIO m
  )
  => ProtoInfo -> blk -> Cycle -> Baker -> Maybe BakerDetails -> Bool -> ExceptT KilnRpcError m (AppSerializable ())
updateDelegateDetails protoInfo headBlock headCycle baker details isInternal = do
  let
    headHash = headBlock ^. hash
    headLvl = headBlock ^. level
    pkh = _baker_publicKeyHash baker
    headFitness = headBlock ^. fitness

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

      j_pti <- catchError
          (Just . Json <$> nodeQueryDataSource (NodeQuery_ParticipationInfo headHash headLvl delegatePkh))
          (\_ -> pure $ _bakerDetails_participationInfo =<< details) -- reuse known data (if any)

      let
        gracePeriod = _cacheDelegateInfo_gracePeriod di

        updateDetails :: AppSerializable ()
        updateDetails = do
          existingData <- project
            ( BakerDetails_publicKeyHashField
            -- We also need to fetch the current 'missedRightsInRow' value to
            -- call 'notifyDefault' on consistent data.
            , BakerDetails_missedRightsInRowField
            )
            ( BakerDetails_publicKeyHashField ==. delegatePkh
            &&. BakerDetails_branchField ~> VeryBlockLike_fitnessSelector <=. headFitness
            )

          let
            mkBakerDetails missedRights = BakerDetails
              { _bakerDetails_publicKeyHash = delegatePkh
              , _bakerDetails_branch = mkVeryBlockLike headBlock
              , _bakerDetails_delegateInfo = Just $ Json di
              , _bakerDetails_participationInfo = j_pti
              , _bakerDetails_missedRightsInRow = missedRights
              }
          case nonEmpty existingData of
            Nothing -> void $ insert (mkBakerDetails 0)
            Just brids -> for_ brids $ \(brid, missedRightsCnt) -> do
              let newBakerDetails@BakerDetails{..} = mkBakerDetails missedRightsCnt
              update
                [ BakerDetails_branchField =. _bakerDetails_branch
                , BakerDetails_delegateInfoField =. _bakerDetails_delegateInfo
                , BakerDetails_participationInfoField =. _bakerDetails_participationInfo
                ]
                ( BakerDetails_publicKeyHashField ==. brid)
              notifyDefault newBakerDetails

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

      pure $ [deactivationAlerts, updateDetails] ++ [insufficientFundAlerts | isInternal]

  return $ sequence_ selfDelegateActions
