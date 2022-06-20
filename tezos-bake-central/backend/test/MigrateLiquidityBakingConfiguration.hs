{-# LANGUAGE LambdaCase #-}

module MigrateLiquidityBakingConfiguration where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Test.Tasty
import Test.Tasty.HUnit

import Backend.Process.Baker
import Common.App

testMigrateLiquidityBakingConfigurations :: TestTree
testMigrateLiquidityBakingConfigurations = testGroup "Migrate liquidity baking configurations"
  [ migrate (votefile Jakarta) (ui On) ?= (LiquidityBakingToggleVote_On , votefile Jakarta)
  , migrate noArgs (ui Off) ?= (LiquidityBakingToggleVote_Off, noArgs)
  , migrate (votefile Ithaca) (ui On) ?= (LiquidityBakingToggleVote_On, noArgs)
  , migrate (lqdty On <> votefile Jakarta) uiNotSet ?= (LiquidityBakingToggleVote_On, votefile Jakarta)
  , migrate (lqdty Pass) uiNotSet ?= (LiquidityBakingToggleVote_Pass, noArgs)
  , migrate lqdtyEscape uiNotSet ?= (LiquidityBakingToggleVote_Off, noArgs)
  , migrate lqdtyEscape (ui On) ?= (LiquidityBakingToggleVote_On, noArgs)
  , migrate (lqdtyEscape <> votefile Jakarta) uiNotSet ?= (LiquidityBakingToggleVote_Off, votefile Jakarta)
  , migrate (votefile Ithaca <> lqdty Off) uiNotSet ?= (LiquidityBakingToggleVote_Off, noArgs)
  , migrate (votefile Ithaca) uiNotSet ?= (LiquidityBakingToggleVote_Off, noArgs) -- escape_vote: true => off
  , migrate noArgs uiNotSet ?= (LiquidityBakingToggleVote_Pass, noArgs) -- 'pass' as a default option
  ]

data Proto = Ithaca | Jakarta

noArgs :: [String]
noArgs = []

votefile :: Proto -> [String]
votefile = \case
  Jakarta -> ["--votefile", "test/Votefiles/votefile_jakarta.json"]
  Ithaca ->  ["--votefile", "test/Votefiles/votefile_ithaca.json"]

data LqdtyToggle = On | Off | Pass

lqdtyEscape :: [String]
lqdtyEscape = ["--liquidity-baking-escape-vote"]

lqdty :: LqdtyToggle -> [String]
lqdty tgl = "--liquidity-baking-toggle-vote" : case tgl of
  On   -> ["on"]
  Off  -> ["off"]
  Pass -> ["pass"]

ui :: LqdtyToggle -> Maybe LiquidityBakingToggleVote
ui On = Just LiquidityBakingToggleVote_On
ui Off = Just LiquidityBakingToggleVote_Off
ui Pass = Just LiquidityBakingToggleVote_Pass

uiNotSet :: Maybe LiquidityBakingToggleVote
uiNotSet = Nothing

migrate
  :: [String]
  -> Maybe LiquidityBakingToggleVote
  -> IO (LiquidityBakingToggleVote, [String])
migrate = flip migrateBakerCustomArgs

(?=) :: (Eq a, Show a) => IO a -> a -> TestTree
action ?= expected = testCase "" $ do
  res <- action
  res @?= expected
