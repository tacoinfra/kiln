{-# language TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Backend.PersistDefinitions where

import Database.Groundhog.TH (groundhog)
import Database.Groundhog.TH.Settings

persistDefinitions :: PersistDefinitions
persistDefinitions = [groundhog|
  - embedded: ProtoInfo
  - entity: ProtocolIndex
    autoKey: null
    keys:
      - name: ProtocolIndexKey
        default: true
    constructors:
      - name: ProtocolIndex
        uniques:
          - name: ProtocolIndexKey
            type: primary
            fields:
              - _protocolIndex_chainId
              - _protocolIndex_hash

  - primitive: VotingPeriodKind
  - primitive: Ballot
  - embedded: Ballots
  - entity: Amendment
    autoKey: null
    constructors:
      - name: Amendment
        uniques:
          - name: Amendment_period
            type: primary
            fields: [_amendment_period, _amendment_chainId, _amendment_votingPeriod]
  - embedded: PeriodVote
  - entity: PeriodProposal
    constructors:
      - name: PeriodProposal
        uniques:
          - name: PeriodProposal_hash
            type: constraint
            fields: [_periodProposal_hash, _periodProposal_chainId, _periodProposal_votingPeriod]
  - entity: BakerProposal
    autoKey: null
    constructors:
      - name: BakerProposal
        fields:
          - name: _bakerProposal_proposal
            reference:
              table: PeriodProposal
              onDelete: cascade
        uniques:
          - name: BakerProposal_key
            type: primary
            fields: [_bakerProposal_pkh, _bakerProposal_proposal]
  - entity: BakerVote
    autoKey: null
    constructors:
      - name: BakerVote
        fields:
          - name: _bakerVote_proposal
            reference:
              table: PeriodProposal
              onDelete: cascade
        uniques:
          - name: BakerVote_key
            type: primary
            fields: [_bakerVote_pkh, _bakerVote_proposal]
  - entity: PeriodTestingVote
    autoKey: null
    constructors:
      - name: PeriodTestingVote
        fields:
          - name: _periodTestingVote_proposal
            reference:
              table: PeriodProposal
              onDelete: cascade
  - primitive: TestChainStatus
  - entity: PeriodTesting
    autoKey: null
    constructors:
      - name: PeriodTesting
        fields:
          - name: _periodTesting_proposal
            reference:
              table: PeriodProposal
              onDelete: cascade
  - entity: PeriodPromotionVote
    autoKey: null
    constructors:
      - name: PeriodPromotionVote
        fields:
          - name: _periodPromotionVote_proposal
            reference:
              table: PeriodProposal
              onDelete: cascade
  - primitive: SigningCurve
  - entity: ConnectedLedger
    autoKey: null
    constructors:
      - name: ConnectedLedger
        fields:
          - name: _connectedLedger_forceConnectivityCheck
            type: Bool
            default: "False"
  - embedded: SecretKey
  - entity: LedgerAccount
    autoKey: null
  - entity: Accusation
    autoKey: null
    constructors:
      - name: Accusation
        uniques:
          - name: Accusation_hash
            type: primary
            fields: [_accusation_hash, _accusation_blockHash]
    keys:
      - name: Accusation_hash
        default: true
  - entity: BlockTodo
    autoKey: null
    constructors:
      - name: BlockTodo
        uniques:
          - name: BlockTodo_hash
            type: primary
            fields: [_blockTodo_hash]
    keys:
      - name: BlockTodo_hash
        default: true
  - embedded: DeletableRow
  - entity: BakerDaemon
    constructors:
      - name: BakerDaemon
  - entity: BakerDaemonInternal
    autoKey: null
    keys:
      - name: BakerDaemonInternalId
        default: true
    constructors:
      - name: BakerDaemonInternal
        uniques:
          - name: BakerDaemonInternalId
            type: primary
            fields: [_bakerDaemonInternal_id]
  - embedded: BakerDaemonInternalData
  - entity: Node
    constructors:
      - name: Node
  - entity: NodeExternal
    autoKey: null
    keys:
      - name: NodeExternalId
        default: true
    constructors:
      - name: NodeExternal
        uniques:
          - name: NodeExternalId
            type: primary
            fields: [_nodeExternal_id]
  - embedded: NodeExternalData
  - entity: NodeInternal
    autoKey: null
    keys:
      - name: NodeInternalId
        default: true
    constructors:
      - name: NodeInternal
        uniques:
          - name: NodeInternalId
            type: primary
            fields: [_nodeInternal_id]
  - entity: ProcessData
  - primitive: ProcessState
  - primitive: ProcessControl
  - entity: NodeDetails
    autoKey: null
    keys:
      - name: NodeDetailsId
        default: true
    constructors:
      - name: NodeDetails
        uniques:
          - name: NodeDetailsId
            type: primary
            fields: [_nodeDetails_id]
  - embedded: NodeDetailsData
  - entity: PublicNodeConfig
    constructors:
    - name: PublicNodeConfig
      uniques:
        - name: _publicnodeconfig_uniqueness
          type: constraint
          fields: [_publicNodeConfig_source]
  - entity: PublicNodeHead
    constructors:
    - name: PublicNodeHead
      uniques:
        - name: _publicnodehead_uniqueness
          type: constraint
          fields: [_publicNodeHead_source, _publicNodeHead_chain]
  - embedded: BakeEfficiency
  - embedded: NetworkStat
  - entity: Baker
    autoKey: null
    keys:
      - name: BakerKey
        default: true
    constructors:
      - name: Baker
        uniques:
          - name: BakerKey
            type: primary
            fields: [_baker_publicKeyHash]
  - embedded: BakerData
  - entity: BakerDetails
    autoKey: null
    keys:
     - name: BakerDetailsKey
       default: true
    constructors:
     - name: BakerDetails
       uniques:
        - name: BakerDetailsKey
          type: primary
          fields: [_bakerDetails_publicKeyHash]
  - entity: BakerRightsCycleProgress
    constructors:
      - name: BakerRightsCycleProgress
        uniques:
          - name: _bakerRightsCycleProgress_branch
            type: constraint
            fields: [_bakerRightsCycleProgress_chainId, _bakerRightsCycleProgress_publicKeyHash, _bakerRightsCycleProgress_branch]
  - entity: BakerRight
    constructors:
      - name: BakerRight
        uniques:
          - name: _bakerRights_right
            type: constraint
            fields: [_bakerRight_branch, _bakerRight_level, _bakerRight_right]
  - embedded: VeryBlockLike
  - entity: Notificatee
    constructors:
      - name: Notificatee
        uniques:
          - name: _notificatee_uniqueness
            type: constraint
            fields: [_notificatee_email]
  - primitive: SmtpProtocol
  - entity: MailServerConfig
    constructors:
      - name: MailServerConfig
        uniques:
          - name: _mailserverconfig_uniqueness
            type: constraint
            fields:
              - _mailServerConfig_hostName
              - _mailServerConfig_portNumber
              - _mailServerConfig_smtpProtocol
              - _mailServerConfig_userName
              - _mailServerConfig_password
        fields:
          - name: _mailServerConfig_enabled
            type: Bool
            default: "True"
  - primitive: RightKind
  - primitive: UpgradeCheckError
  - primitive: PublicNode
  - primitive: NamedChain
  - entity: ErrorLog

  - entity: ErrorLogBadNodeHead
    autoKey: null
    keys:
      - name: ErrorLogBadNodeHeadId
        default: true
    constructors:
      - name: ErrorLogBadNodeHead
        uniques:
          - name: ErrorLogBadNodeHeadId
            type: primary
            fields: [_errorLogBadNodeHead_log]
  - entity: ErrorLogBakerNoHeartbeat
    autoKey: null
    keys:
      - name: ErrorLogBakerNoHeartbeatId
        default: true
    constructors:
      - name: ErrorLogBakerNoHeartbeat
        uniques:
          - name: ErrorLogBakerNoHeartbeatId
            type: primary
            fields: [_errorLogBakerNoHeartbeat_log]
  - entity: ErrorLogBakerLedgerDisconnected
    autoKey: null
    keys:
      - name: ErrorLogBakerLedgerDisconnectedId
        default: true
    constructors:
      - name: ErrorLogBakerLedgerDisconnected
        uniques:
          - name: ErrorLogBakerLedgerDisconnectedId
            type: primary
            fields: [_errorLogBakerLedgerDisconnected_log]
  - entity: ErrorLogInaccessibleNode
    autoKey: null
    keys:
      - name: ErrorLogInaccessibleNodeId
        default: true
    constructors:
      - name: ErrorLogInaccessibleNode
        uniques:
          - name: ErrorLogInaccessibleNodeId
            type: primary
            fields: [_errorLogInaccessibleNode_log]
  - entity: ErrorLogBakerAccused
    autoKey: null
    keys:
      - name: ErrorLogBakerAccusedId
        default: true
    constructors:
      - name: ErrorLogBakerAccused
        uniques:
          - name: ErrorLogBakerAccusedId
            type: primary
            fields: [_errorLogBakerAccused_log]
  - entity: ErrorLogBakerDeactivated
    autoKey: null
    keys:
      - name: ErrorLogBakerDeactivatedId
        default: true
    constructors:
      - name: ErrorLogBakerDeactivated
        uniques:
          - name: ErrorLogBakerDeactivatedId
            type: primary
            fields: [_errorLogBakerDeactivated_log]
  - entity: ErrorLogBakerDeactivationRisk
    autoKey: null
    keys:
      - name: ErrorLogBakerDeactivationRiskId
        default: true
    constructors:
      - name: ErrorLogBakerDeactivationRisk
        uniques:
          - name: ErrorLogBakerDeactivationRiskId
            type: primary
            fields: [_errorLogBakerDeactivationRisk_log]
  - entity: ErrorLogInsufficientFunds
    autoKey: null
    keys:
      - name: ErrorLogInsufficientFundsId
        default: true
    constructors:
      - name: ErrorLogInsufficientFunds
        uniques:
          - name: ErrorLogInsufficientFundsId
            type: primary
            fields: [_errorLogInsufficientFunds_log]
  - entity: ErrorLogNodeWrongChain
    autoKey: null
    keys:
      - name: ErrorLogNodeWrongChainId
        default: true
    constructors:
      - name: ErrorLogNodeWrongChain
        uniques:
          - name: ErrorLogNodeWrongChainId
            type: primary
            fields: [_errorLogNodeWrongChain_log]
  - entity: ErrorLogNodeInvalidPeerCount
    autoKey: null
    keys:
      - name: ErrorLogNodeInvalidPeerCountId
        default: true
    constructors:
      - name: ErrorLogNodeInvalidPeerCount
        uniques:
          - name: ErrorLogNodeInvalidPeerCountId
            type: primary
            fields: [_errorLogNodeInvalidPeerCount_log]
  - entity: ErrorLogNetworkUpdate
    autoKey: null
    keys:
      - name: ErrorLogNetworkUpdateId
        default: true
    constructors:
      - name: ErrorLogNetworkUpdate
        uniques:
          - name: ErrorLogNetworkUpdateId
            type: primary
            fields: [_errorLogNetworkUpdate_log]
  - entity: ErrorLogBakerMissed
    autoKey: null
    keys:
      - name: ErrorLogBakerMissedId
        default: true
    constructors:
      - name: ErrorLogBakerMissed
        uniques:
          - name: ErrorLogBakerMissedId
            type: primary
            fields: [_errorLogBakerMissed_log]
  - entity: ErrorLogVotingReminder
    autoKey: null
    keys:
      - name: ErrorLogVotingReminderId
        default: true
    constructors:
      - name: ErrorLogVotingReminder
        uniques:
          - name: ErrorLogVotingReminderId
            type: primary
            fields: [_errorLogVotingReminder_log]
  - primitive: InternalNodeFailureReason
  - entity: ErrorLogInternalNodeFailed
    autoKey: null
    keys:
      - name: ErrorLogInternalNodeFailedId
        default: true
    constructors:
      - name: ErrorLogInternalNodeFailed
        uniques:
          - name: ErrorLogInternalNodeFailedId
            type: primary
            fields: [_errorLogInternalNodeFailed_log]
  - entity: RawCacheEntry
    constructors:
     - name: RawCacheEntry
       uniques:
        - name: _rawCacheEntry_uniqueness
          type: constraint
          fields:
           - _rawCacheEntry_chainId
           - _rawCacheEntry_key
  - entity: TelegramConfig
    constructors:
    - name: TelegramConfig
      uniques:
      - name: _telegramConfig_uniqueness
        type: constraint
        fields: [_telegramConfig_botApiKey]
  - entity: TelegramMessageQueue
  - entity: TelegramRecipient
  - entity: SnapshotMeta
  - entity: UpstreamVersion
  - embedded: RightNotificationLimit
  - entity: RightNotificationSettings
    autoKey: null
    constructors:
    - name: RightNotificationSettings
      uniques:
      - name: RightNotificationSettingsId
        type: primary
        fields: [_rightNotificationSettings_rightKind]
  - entity: CacheBakingRights
    autoKey: null
    constructors:
      - name: CacheBakingRights
        uniques:
          - name: CacheBakingRights_context
            type: primary
            fields: [_cacheBakingRights_context, _cacheBakingRights_level]
  - entity: CacheEndorsingRights
    autoKey: null
    constructors:
      - name: CacheEndorsingRights
        uniques:
          - name: CacheEndorsingRights_context
            type: primary
            fields: [_cacheEndorsingRights_context, _cacheEndorsingRights_level]
|]
