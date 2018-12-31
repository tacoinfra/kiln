{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Backend.Workers.Baker where

import Prelude hiding (cycle)

import Control.Arrow ((&&&))
import Control.Lens (anyOf, set, (<>=), (<<>=), (%=), ix, over, _4, ifoldMap, at, (.=), FoldableWithIndex)
import Control.Exception (handle, SomeException)
import Control.Concurrent.STM (atomically)
import Control.Monad (guard, mzero)
import Control.Monad.Except (MonadError, runExceptT, ExceptT(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger, logDebug, logDebugSH, logErrorSH, LoggingT(..))
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
import Reflex (fforMaybe)
import Rhyolite.Backend.DB (runDb, selectMap)
import Rhyolite.Backend.DB.PsqlSimple (executeQ, queryQ, In(..))
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Schema (Id (..))
import Rhyolite.Schema (Id (..), Json(..))
import Safe (maximumDef, minimumDef)

import Tezos.Types
import Tezos.Operation

import Backend.Config (AppConfig (..), HasAppConfig)
import Backend.Alerts
import Backend.CachedNodeRPC
import Backend.Common (worker')
import Backend.Config (AppConfig (..))
import Backend.Schema
import Backend.STM (atomicallyWith)
import Backend.Alerts (clearMissedBake, reportMissedBake)
import Common (curryMap)
import Common.Schema
import ExtraPrelude

import Data.Align
import Data.These (These(..), these)
import Algebra.Lattice ((/\))

import Rhyolite.Backend.Schema.Class (DefaultKeyId, toIdData, fromIdData)

-- TODO: This only loops through one cycle at a time, per block;  we don't need to wait that long (although it may still end up doing the right thing eventually)

bakerRightsWorker
  :: forall m. MonadIO m
  => NodeDataSource
  -> m (IO ())
bakerRightsWorker nds = worker' $ (<* waitForNewHead nds) $ runLoggingEnv (_nodeDataSource_logger nds) $ do
  (protoInfo, headM) <- liftIO $ atomically $
    (,) <$> waitForParams nds <*> dataSourceHead nds

  let
    db = _nodeDataSource_pool nds

    -- the levels returned by cycleStartHashes are the levels in the cycle that
    -- determines those rights.  the level that those rights are applied to are
    -- in the future of that.
    rightsLookAhead :: RawLevel
    rightsLookAhead = firstLevelInCycle protoInfo (1 + _protoInfo_preservedCycles protoInfo) - 1 -- cycle starts are offset by 1

  res <- runExceptT $ for_ headM $ \headBlock -> flip runReaderT nds $ do
    $(logDebug) "Update baker cycle."
    let
      chainId = _nodeDataSource_chain nds
      headHash :: BlockHash = headBlock ^. hash
    cycleHashes :: [RightsCycleInfo] <- fmap (fromMaybe []) $ atomicallyWith $ cycleStartHashes headHash
    let
      minCycle = minimumDef 0 $ fmap _rightsCycleInfo_cycle cycleHashes
      maxCycle = maximumDef (-1) $ fmap _rightsCycleInfo_cycle cycleHashes
      -- well just swizzle these around
      cycleHashesByCycle = Map.fromList $ (_rightsCycleInfo_cycle &&& id) <$> cycleHashes

    $(logDebug) $ "BAKER baseline " <> tshow (_rightsCycleInfo_branch <$> cycleHashes)

    -- * compute the list of rights we "want" to have and the list we actually have; their difference is the rights we need
    -- * then actually obtain the rights for all bakers at the oldest cycle we still want.
    needProgress :: MonoidalMap (Cycle, PublicKeyHash) (Max BakerRightsCycleProgress) <- runDb (Identity db) $ do
      bakerPKHs :: [PublicKeyHash] <- project (Baker_publicKeyHashField) (Baker_deletedField ==. False)
      let
        inBakerPKHs = In bakerPKHs
        inCycleHashes = In $ _rightsCycleInfo_branch <$> cycleHashes

      -- hot table, rewrite using psql-simple to avoid ==.
      bakerRightsCycleProgress' :: Map (Id BakerRightsCycleProgress) BakerRightsCycleProgress <-
        [queryQ|
          SELECT "id", "chainId", "branch", "publicKeyHash", "cycle", "progress"
          FROM "BakerRightsCycleProgress"
          WHERE "publicKeyHash" in ?inBakerPKHs
            AND "chainId" = ?chainId
            AND "branch" in ?inCycleHashes
            AND "cycle" BETWEEN ?minCycle AND ?maxCycle
        |] <&> Map.fromList . fmap (\(i, ch, b, p, cy, pr) -> (i, BakerRightsCycleProgress ch b p cy pr))

      -- here's what we've got:
      let
        haveProgress :: MonoidalMap (Cycle,  PublicKeyHash) (Max BakerRightsCycleProgress)
        haveProgress = flip foldMap bakerRightsCycleProgress' $
          \p -> MMap.singleton (_bakerRightsCycleProgress_cycle &&& _bakerRightsCycleProgress_publicKeyHash $ p) (Max p)

      -- this is all of the progress we could possibly want.  we indicate that the progress we've made is none by using the level just before the cycle starts.
      return $ (haveProgress <>) $ MMap.fromList $ do
            pkh <- bakerPKHs
            cycleHash <- cycleHashes
            let
              cycle = _rightsCycleInfo_cycle cycleHash
              v = BakerRightsCycleProgress
                { _bakerRightsCycleProgress_chainId = chainId
                , _bakerRightsCycleProgress_branch = _rightsCycleInfo_branch cycleHash
                , _bakerRightsCycleProgress_publicKeyHash = pkh
                , _bakerRightsCycleProgress_cycle = cycle
                , _bakerRightsCycleProgress_progress = rightsLookAhead + _rightsCycleInfo_minLevel cycleHash - 1
                }
            return ((cycle, pkh), (Max v))

    let
      pkhs :: Set PublicKeyHash
      pkhs = Set.fromList $ fmap (_bakerRightsCycleProgress_publicKeyHash . getMax) $ toList needProgress
      -- drop the already completed bakers.
      unfinished :: MonoidalMap Cycle (NonEmpty BakerRightsCycleProgress)
      unfinished = MMap.mapMaybe (nonEmpty . toList) $ curryMap $ flip MMap.mapMaybe needProgress $ \(Max p) -> do
        cycle' <- Map.lookup (_bakerRightsCycleProgress_cycle p) cycleHashesByCycle
        -- if we're already at maxLevel, then we're done here.
        guard (_bakerRightsCycleProgress_progress p < rightsLookAhead + _rightsCycleInfo_maxLevel cycle')
        return p


      mNextUnfinished :: Maybe (NonEmpty BakerRightsCycleProgress, RightsCycleInfo) = do
        (cycle, x) <- Map.lookupMin $ MMap.getMonoidalMap unfinished
        cycle' <- Map.lookup cycle cycleHashesByCycle
        return (x, cycle')

    $(logDebugSH) ("Baker rights TODO:" :: Text, unfinished)
    for_ mNextUnfinished $ \((aBakerRight :| moreUnfinished), aCycleInfo) -> for_ [minimumDef (_bakerRightsCycleProgress_progress aBakerRight) $ _bakerRightsCycleProgress_progress <$> moreUnfinished .. rightsLookAhead + _rightsCycleInfo_maxLevel aCycleInfo] $ \lvl -> do
      -- At this point, our use of the earlier queried BakerRightsCycleProgress is "useless",  we've previously made at least that much progress, so it tells us which we should work on, 
      reqBakers <- nodeQueryDataSource $ NodeQuery_BakingRights headHash lvl
      reqEndorsers <- nodeQueryDataSource $ NodeQuery_EndorsingRights headHash lvl
      let
        pri1baker :: Maybe BakingRights
        pri1baker = fmap NonEmpty.head . nonEmpty . (filter $ (flip Set.member pkhs . _bakingRights_delegate) /\ (== 0) . _bakingRights_priority) $ toList reqBakers
        endorsers :: Seq EndorsingRights
        endorsers = Seq.filter (flip Set.member pkhs . _endorsingRights_delegate) reqEndorsers
        branch :: BlockHash
        branch = _rightsCycleInfo_branch aCycleInfo

        bakerRightCycleInfo :: PublicKeyHash -> BakerRightsCycleProgress
        bakerRightCycleInfo pkh = BakerRightsCycleProgress
          { _bakerRightsCycleProgress_chainId = chainId
          , _bakerRightsCycleProgress_branch = branch
          , _bakerRightsCycleProgress_publicKeyHash = pkh
          , _bakerRightsCycleProgress_cycle = _rightsCycleInfo_cycle aCycleInfo
          , _bakerRightsCycleProgress_progress = lvl
          }
        bakerRights :: Maybe (Id BakerRightsCycleProgress) -> PublicKeyHash -> [BakerRight]
        bakerRights pid pkh =
          [ BakerRight pid' lvl RightKind_Baking Nothing
            | pid' <- toList pid
            , pri1' <- toList pri1baker
            , _bakingRights_delegate pri1' == pkh
            ] ++
          [ BakerRight pid' lvl RightKind_Endorsing (Just $ length $ _endorsingRights_slots end)
            | pid' <- toList pid
            , end <- toList endorsers
            , _endorsingRights_delegate end == pkh
            ]

      when (mod lvl 100 == 0) $ $(logDebug) ("bakerrights working lvl:" <> tshow (unRawLevel lvl))
      runDb (Identity db) $ for_ pkhs $ \pkh -> do
        let
          newProgress = bakerRightCycleInfo pkh
        progress' :: [(Id BakerRightsCycleProgress, BakerRightsCycleProgress)] <- Map.toList <$> selectMap BakerRightsCycleProgressConstructor  -- BakerRightsCycleProgressConstructor
          ( BakerRightsCycleProgress_publicKeyHashField `in_` [pkh]
          &&. BakerRightsCycleProgress_chainIdField `in_` [chainId]
          &&. BakerRightsCycleProgress_branchField `in_` [branch]
          )
        progressId :: Maybe (Id BakerRightsCycleProgress) <- case nonEmpty progress' of
          Nothing -> Just . toId <$> insert newProgress -- assert lvl == _rightsCycleInfo_minLevel
          Just ((pId, p):|_)
            -- | _bakerRightsCycleProgress_progress < lvl-1 -> TODO sulk
            | _bakerRightsCycleProgress_progress p < lvl -> do
              _ <- [executeQ|
                UPDATE "BakerRightsCycleProgress"
                SET progress = ?lvl
                WHERE "id" = ?pId
                |]

              return $ Just pId
            | otherwise -> return Nothing -- already have this progress, do nothing.
        rights <- for (bakerRights progressId pkh) $ \r -> insert r *> return r
        traverse_ notify $ Notify_BakerRightsProgress <$> progressId <*> pure newProgress <*> pure rights
        return ()

  case res of
    Right _ -> pure ()
    Left (err :: CacheError) -> $(logErrorSH) ("bakerRightsWorker" :: String, err)

  $(logDebug) $ "BAKERRIGHTSWORKER STEP" <> tshow res

bakerWorker
  :: forall m. MonadIO m
  => AppConfig
  -> NodeDataSource
  -> m (IO ())
bakerWorker appConfig nds = worker' $ (<* waitForNewHead nds) $ runLoggingEnv (_nodeDataSource_logger nds) $ do
  (protoInfo, headM) :: (ProtoInfo, Maybe VeryBlockLike) <- liftIO $ atomically $ do
    params <- waitForParams nds
    dsh <- dataSourceHead nds
    return (params, dsh)

  let
    db = _nodeDataSource_pool nds

  res <- runExceptT $ for_ headM $ \headBlock -> flip runReaderT nds $ do
    let
    currentState :: [(Baker, Maybe BakerDetails)] <- runDb (Identity db) $ do
      bakers :: Map PublicKeyHash Baker <- Map.fromList <$> project (Baker_publicKeyHashField, BakerConstructor) (Baker_deletedField ==. False)
      details :: Map PublicKeyHash BakerDetails <- Map.fromList <$> project
        (BakerDetails_publicKeyHashField, BakerDetailsConstructor)
        (BakerDetails_publicKeyHashField `in_` (Map.keys bakers))
      return $ catMaybes $ toList $ alignWith (these (Just . ($ Nothing) . (,)) (const Nothing) (curry (Just . fmap Just))) bakers details

    wantedActions <- for currentState $ \(baker, details) ->
      getWantedAction protoInfo headBlock baker details

    -- beware of the jellyfish
    runDb (Identity db) $ runReaderT (sequence_ wantedActions) appConfig

  case res of
    Right () -> $(logDebug) $ "bakerWorker DONE"
    Left (err :: CacheError) -> $(logErrorSH) ("bakerWorker" :: String, err)


-- separating the monad that can do RPC(mPrepare) from the one that can do
-- SQL(mCommit) makes it a little easier to set up the transactions to succeed.
--
-- Make sure that the mCommit action has enough information to bail out or do
-- nothing if the data gathered in mPrepare can be stale
{-# INLINE getWantedAction #-}
getWantedAction
  :: forall mPrepare e rP mCommit rC blk.
  ( BlockLike blk
  , MonadIO mPrepare, MonadReader rP mPrepare, HasNodeDataSource rP, MonadLogger mPrepare, MonadError e mPrepare, AsCacheError e
  , MonadIO mCommit, MonadReader rC mCommit, HasAppConfig rC, MonadLogger mCommit, PostgresLargeObject mCommit, PersistBackend mCommit
  )
  => ProtoInfo -> blk -> Baker -> Maybe BakerDetails -> mPrepare (mCommit ())
getWantedAction protoInfo headBlock baker details = do
  let
    headHash = headBlock ^. hash
    headPred = headBlock ^. predecessor
    headLvl = headBlock ^. level
    headCycle = levelToCycle protoInfo headLvl
    pkh = _baker_publicKeyHash baker
    headFitness = headBlock ^. fitness
    -- for each baker; follow the branch it was on previously to the new head
    -- check at each level to see if the baker had rights there;
    -- if so, examine the block to see if they exercized those rights
    -- if not, report an error; if so, clear an error.
    detailsBranch :: BlockHash = maybe headPred (view hash . _bakerDetails_branch) details
  -- we're only interested in the "good" branch, so we discard the part only on the baker's current branch
  -- enumerateBranches reutrns a maybe if it coulnd't find enough history
  -- to reasonably answer the question; so we'll just short curcuit and
  -- behave as if we have never run before.
  -- we don't actually need to know the hashes; headHash is sufficient, but we do need to know the levels.
  headBranch :: [(RawLevel, BlockHash)] <- atomicallyWith
    $ zipWith (,) [headLvl, pred headLvl .. 1]
    . fst
    . fromMaybe ([headHash], [])
    <$> enumerateBranches headHash detailsBranch
  $(logDebugSH) ("getWantedAction" :: Text, baker, headHash, headLvl, headBranch)
  bakingEndorsingAlerts :: [mCommit ()] <- for headBranch $ \(lvl, thisHash) -> do
    bakingRights <- nodeQueryDataSource $ NodeQuery_BakingRights headHash lvl
    bakingAlerts :: [mCommit ()]
                 <- whenM (any ((== 0) . _bakingRights_priority /\ (== _baker_publicKeyHash baker) . _bakingRights_delegate) bakingRights) $ do
      thisBlock <- nodeQueryDataSource $ NodeQuery_Block thisHash
      let action = (bool reportMissedBake clearMissedBake
                   (_blockMetadata_baker (_block_metadata thisBlock) == _baker_publicKeyHash baker))
                    (headBlock ^. fitness)
                    RightKind_Baking
                    (baker ^. baker_publicKeyHash)
                    lvl
      return $ pure $ action

    -- endorsements *on* this block are *of* the previos block
    endorsers <- nodeQueryDataSource $ NodeQuery_EndorsingRights headHash (lvl - 1)
    endorsingAlerts :: [mCommit ()]
                    <- whenM (any ((== _baker_publicKeyHash baker) . _endorsingRights_delegate) endorsers) $ do
      thisBlock <- nodeQueryDataSource $ NodeQuery_Block thisHash
      let action = (bool reportMissedBake clearMissedBake (anyOf (block_operations . traverse . traverse . operation_contents . traverse . _OperationContents_Endorsement . operationContentsEndorsement_metadata . endorsementMetadata_delegate) (== _baker_publicKeyHash baker) thisBlock))
                   (headBlock ^. fitness)
                   RightKind_Endorsing
                   (baker ^. baker_publicKeyHash)
                   (lvl - 1)
      return $ pure $ action

    return $ sequence_ $ bakingAlerts <> endorsingAlerts

  di <- nodeQueryDataSource (NodeQuery_DelegateInfo headHash headLvl pkh)
  let
    gracePeriod = _cacheDelegateInfo_gracePeriod di

    updateDetails :: mCommit ()
    updateDetails = do
      existingIds <- project BakerDetails_publicKeyHashField
        ( BakerDetails_publicKeyHashField ==. pkh
        &&. BakerDetails_branchField ~> VeryBlockLike_fitnessSelector <=. headFitness
        )

      let
        newVal = BakerDetails
          { _bakerDetails_publicKeyHash = pkh
          -- , _bakerDetails_nextBakeRights = _bakingRights_level <$> Map.lookup pkh bakingRights
          -- , _bakerDetails_nextEndorseRights = _endorsingRights_level <$> Map.lookup pkh endorsingRights
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
      notify $ mkDefaultNotify newVal

    -- Within a single run of a kiln instance, the fitness of blocks we observe is non-decreasing,
    -- but there might be multiple instances or resets, so we can only clear an error when a fitter block claims it's gone.
    deactivationAlerts :: mCommit ()
    deactivationAlerts =
      if _cacheDelegateInfo_deactivated di
        then reportBakerDeactivated pkh protoInfo headFitness
        else do
          clearBakerDeactivated pkh headFitness
          if (1 >= gracePeriod - headCycle)
            then reportBakerDeactivationRisk pkh gracePeriod headCycle protoInfo headFitness
            else clearBakerDeactivationRisk pkh headFitness

  return $ sequence_ $
    [ deactivationAlerts
    , updateDetails
    ] ++ bakingEndorsingAlerts
