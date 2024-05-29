module Backend.Common.Ledger
  ( LedgerQuery (..)
  , LedgerQueryType (..)
  ) where

data LedgerQueryType
  = LedgerQueryType_PollLedger
  | LedgerQueryType_ShowLedger
  | LedgerQueryType_ImportKey
  | LedgerQueryType_SetupToBake
  | LedgerQueryType_RegisterDelegate
  | LedgerQueryType_SetHWM
  | LedgerQueryType_CheckHWM
  | LedgerQueryType_Vote
  | LedgerQueryType_Stake
  | LedgerQueryType_Unstake
  | LedgerQueryType_FinalizeUnstake

data LedgerQuery m = LedgerQuery
  { _ledgerQuery_type :: LedgerQueryType
  , _ledgerQuery_action :: m ()
  }
