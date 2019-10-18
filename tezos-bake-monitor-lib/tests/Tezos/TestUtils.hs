{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tezos.TestUtils where

import Data.Aeson(ToJSON,FromJSON,Value, encode, eitherDecode)
import Data.Algorithm.DiffContext (prettyContextDiff, getContextDiff)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (assertBool, testCase)
import qualified Text.PrettyPrint as Doc
import Text.Show.Pretty (ppShow)

-- The person that wrote this really should put it in a library...
-- https://github.com/feuerbach/tasty/issues/226#issuecomment-508868067
(@?~)
  :: (Eq a, Show a)
  => a
  -> a
  -> IO ()
got @?~ expected = assertBool (show diff) (got == expected)
  where
    gotPurdy = lines $ ppShow got
    expectedPurdy = lines $ ppShow expected
    diff = prettyContextDiff "Got" "Expected" Doc.text (getContextDiff 3 gotPurdy expectedPurdy)

-- The purpose of this test is to do a round trip test between our haskell type and JSON output by a real
-- Node RPC call to make sure that we can decode and encode real output properly.
--
-- Note, our ToJSON formats don't actually match the output from the NodeRPC because we
-- do not have omitNothingFields set in our TH options, so we can't do a true round trip
-- and have to cheat a little. We round trip from haskell to bytes to haskell and make sure
-- that we get the same thing and then decode the file and make sure it matches our haskell
-- value.
--
-- This wont pick up instances where we are not parsing something from the RPC json! But if we don't
-- care about it in any of our haskell code, this probably doesn't matter
aesonRoundTripTest :: forall a. (Eq a, Show a, ToJSON a, FromJSON a) => TestName -> FilePath -> a -> TestTree
aesonRoundTripTest tn fp a = testCase tn $ do
  -- Test that round tripping from haskell and back gets back to the same thing
  eitherDecode (encode a) @?~ Right a
  -- Grab the actual file
  flbs <- LBS.readFile fp
  -- And then make sure decoding that file lines up to our a.
  eitherDecode flbs @?~ Right a
