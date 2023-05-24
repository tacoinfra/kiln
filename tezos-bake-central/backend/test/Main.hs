module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Accusation (testAccusations)
import SnapshotMetadata (testSnapshotMetadata)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Backend tests"
  [ testAccusations
  , testSnapshotMetadata
  ]
