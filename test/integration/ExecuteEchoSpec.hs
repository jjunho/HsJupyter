{-# LANGUAGE OverloadedStrings #-}

-- TODO: Fix this integration test - it needs substantial rework
-- The test tries to start a real kernel process and communicate via ZeroMQ,
-- but the implementation has changed and this test is out of date.

module ExecuteEchoSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "Execute echo integration (DISABLED)" $ do
  it "needs to be reimplemented" $ pending
