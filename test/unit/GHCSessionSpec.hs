-- | Unit tests for GHC session state management
-- Tests session persistence and state management
module GHCSessionSpec where

import Control.Concurrent.STM (atomically)
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Hspec
import HsJupyter.Runtime.GHCSession
import HsJupyter.Runtime.GHCRuntime (defaultGHCConfig)

spec :: Spec
spec = describe "GHCSession" $ do
  describe "session state management" $ do
    it "creates new session with empty state" $ do
      session <- atomically $ newGHCSession defaultGHCConfig
      bindings <- atomically $ listBindings session
      modules <- atomically $ listImportedModules session
      
      bindings `shouldBe` []
      modules `shouldBe` []
      sessionId session `shouldBe` "default-session"

    it "can add and retrieve bindings" $ do
      session <- atomically $ newGHCSession defaultGHCConfig
      
      atomically $ do
        addBinding session "x"
        addBinding session "myFunction"
      
      bindings <- atomically $ listBindings session
      Set.fromList bindings `shouldBe` Set.fromList ["x", "myFunction"]

    it "can remove bindings" $ do
      session <- atomically $ newGHCSession defaultGHCConfig
      
      atomically $ do
        addBinding session "x"
        addBinding session "y"
        removeBinding session "x"
      
      bindings <- atomically $ listBindings session
      bindings `shouldBe` ["y"]

    it "can add and retrieve imported modules" $ do
      session <- atomically $ newGHCSession defaultGHCConfig
      
      atomically $ do
        addImportedModule session "Data.List"
        addImportedModule session "Data.Map"
      
      modules <- atomically $ listImportedModules session
      modules `shouldBe` ["Data.Map", "Data.List"]  -- Reverse order due to prepending

    it "can clean up session state" $ do
      session <- atomically $ newGHCSession defaultGHCConfig
      
      -- Add some state
      atomically $ do
        addBinding session "x"
        addImportedModule session "Data.List"
      
      -- Clean up
      atomically $ cleanupSession session
      
      -- Verify clean state
      bindings <- atomically $ listBindings session
      modules <- atomically $ listImportedModules session
      
      bindings `shouldBe` []
      modules `shouldBe` []

  describe "binding name extraction" $ do
    it "extracts names from let bindings" $ do
      extractBindingNames "let x = 42" `shouldBe` ["x"]
      extractBindingNames "let   y   = \"hello\"" `shouldBe` ["y"]

    it "extracts names from function definitions" $ do
      extractBindingNames "square x = x * x" `shouldBe` ["square"]
      extractBindingNames "add a b = a + b" `shouldBe` ["add"]

    it "handles empty or non-binding code" $ do
      extractBindingNames "2 + 3" `shouldBe` []
      extractBindingNames "print \"hello\"" `shouldBe` []
      extractBindingNames "" `shouldBe` []

    it "handles multiple bindings" $ do
      let multiLineCode = "let x = 1\nlet y = 2"
      extractBindingNames multiLineCode `shouldSatisfy` (\names -> 
        Set.fromList names == Set.fromList ["x", "y"])

    it "ignores comparisons and arrows" $ do
      extractBindingNames "x == y" `shouldBe` []
      extractBindingNames "case x of\n  Just y => y" `shouldBe` []