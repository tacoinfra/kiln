{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Backend.Workers.Baker where

import Prelude hiding (cycle)

import Control.Arrow ((&&&), )
import Control.Lens (set, (<>=), (<<>=), (%=), ix, over, _4, ifoldMap, at, (.=), FoldableWithIndex)
import Control.Exception (handle, SomeException)
import Control.Concurrent.STM (atomically)
import Control.Monad (guard, mzero)
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger, logDebug, logDebugSH, logErrorSH)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (MonadState, execStateT, gets, modify)
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
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Schema (Id (..))
import Safe (maximumDef, minimumDef)

import Tezos.Types

import Backend.CachedNodeRPC
import Backend.Common (worker')
import Backend.Schema
import Backend.STM (atomicallyWith)
import Common (curryMap)
import Common.Schema
import ExtraPrelude

import Data.Align
import Data.These (These(..), these)
import Algebra.Lattice ((/\))

-- TODO: This only loops through one cycle at a time, per block;  we don't need to wait that long (although it may still end up doing the right thing eventually)

bakerWorker
  :: forall m. MonadIO m
  => NodeDataSource
  -> m (IO ())
bakerWorker nds = worker' $ (<* waitForNewHead nds) $ runLoggingEnv (_nodeDataSource_logger nds) $ do
  headM <- liftIO $ atomically $
    waitForParams nds *> dataSourceHead nds

  let db = _nodeDataSource_pool nds
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

      bakerRightsCycleProgress' :: Map (Id BakerRightsCycleProgress) BakerRightsCycleProgress <- selectMap BakerRightsCycleProgressConstructor
        ( BakerRightsCycleProgress_publicKeyHashField `in_` bakerPKHs
        &&. BakerRightsCycleProgress_chainIdField ==. chainId
        &&. BakerRightsCycleProgress_branchField `in_` fmap _rightsCycleInfo_branch cycleHashes
        &&. BakerRightsCycleProgress_cycleField >=. minCycle
        &&. BakerRightsCycleProgress_cycleField <=. maxCycle
        )
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
                , _bakerRightsCycleProgress_progress = _rightsCycleInfo_minLevel cycleHash - 1
                }
            return ((cycle, pkh), (Max v))
    $(logDebug) $ "BAKER-NEED-PROGRESS " <> tshow needProgress

    let
      pkhs :: Set PublicKeyHash
      pkhs = Set.fromList $ fmap (_bakerRightsCycleProgress_publicKeyHash . getMax) $ toList needProgress
      -- drop the already completed bakers.
      unfinished :: MonoidalMap Cycle (NonEmpty BakerRightsCycleProgress)
      unfinished = MMap.mapMaybe (nonEmpty . toList) $ curryMap $ flip MMap.mapMaybe needProgress $ \(Max p) -> do
        cycle' <- Map.lookup (_bakerRightsCycleProgress_cycle p) cycleHashesByCycle
        -- if we're already at maxLevel, then we're done here.
        guard (_bakerRightsCycleProgress_progress p < _rightsCycleInfo_maxLevel cycle')
        return p

      mNextUnfinished :: Maybe (NonEmpty BakerRightsCycleProgress, RightsCycleInfo) = do
        (cycle, x) <- Map.lookupMin $ MMap.getMonoidalMap unfinished
        cycle' <- Map.lookup cycle cycleHashesByCycle
        return (x, cycle')

    $(logDebugSH) ("Baker rights TODO:" :: Text, unfinished)
    for_ mNextUnfinished $ \((aBakerRight :| moreUnfinished), aCycleInfo) -> for_ [minimumDef (_bakerRightsCycleProgress_progress aBakerRight) $ _bakerRightsCycleProgress_progress <$> moreUnfinished .. _rightsCycleInfo_maxLevel aCycleInfo] $ \lvl -> do
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

      runDb (Identity db) $ do
        for_ pkhs $ \pkh -> do
          let
            newProgress = bakerRightCycleInfo pkh
          progress' :: [(Id BakerRightsCycleProgress, BakerRightsCycleProgress)] <- Map.toList <$> selectMap BakerRightsCycleProgressConstructor  -- BakerRightsCycleProgressConstructor
            ( BakerRightsCycleProgress_publicKeyHashField ==. pkh
            &&. BakerRightsCycleProgress_chainIdField ==. chainId
            &&. BakerRightsCycleProgress_branchField ==. branch
            )
          progressId :: Maybe (Id BakerRightsCycleProgress) <- case nonEmpty progress' of
            Nothing -> Just . toId <$> insert newProgress -- assert lvl == _rightsCycleInfo_minLevel
            Just ((pId, p):|_)
              -- | _bakerRightsCycleProgress_progress < lvl-1 -> TODO sulk
              | _bakerRightsCycleProgress_progress p < lvl -> do
                updateId pId
                  [ BakerRightsCycleProgress_progressField =. lvl
                  ]
                return $ Just pId
              | otherwise -> return Nothing -- already have this progress, do nothing.
          rights <- for (bakerRights progressId pkh) $ \r -> insert r *> return r
          traverse_ notify $ Notify_BakerRightsProgress <$> progressId <*> pure newProgress <*> pure rights

  case res of
    Right _ -> pure ()
    Left (err :: CacheError) -> $(logErrorSH) ("bakerWorker" :: String, err)

  $(logDebug) $ "BAKER STEP" <> tshow res
