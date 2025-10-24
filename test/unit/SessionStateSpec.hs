{-# LANGUAGE OverloadedStrings #-}

module SessionStateSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock (secondsToDiffTime)
import Test.Hspec

import HsJupyter.Runtime.SessionState

spec :: Spec
spec = describe "RuntimeSessionState" $ do
  it "initialises with empty state and zero execution count" $ do
    let budget = ResourceBudget
          { rbCpuTimeout = 5
          , rbMemoryLimit = 256 * 1024 * 1024
          , rbTempDirectory = "/tmp/hsjupyter"
          , rbMaxStreamBytes = 1024 * 1024
          }
        state = initialSessionState budget
    rssExecutionCount state `shouldBe` 0
    rssLoadedModules state `shouldBe` Map.empty
    rssBindings state `shouldBe` Map.empty
    rssImports state `shouldBe` []
    rssResourceBudget state `shouldBe` budget

  it "increments execution count deterministically" $ do
    let budget = ResourceBudget 5 0 "/tmp" (1024 * 1024)
        state = initialSessionState budget
        state' = incrementExecutionCount state
        state'' = incrementExecutionCount state'
    rssExecutionCount state' `shouldBe` 1
    rssExecutionCount state'' `shouldBe` 2

  it "marks queue enqueueable only when under capacity" $ do
    enqueueable 3 0 `shouldBe` True
    enqueueable 3 2 `shouldBe` True
    enqueueable 3 3 `shouldBe` False

  describe "binding management" $ do
    it "can store and retrieve bindings" $ do
      let binding = Binding "myVar" "String"
          bindings = Map.fromList [("myVar", binding)]
          budget = ResourceBudget 5 0 "/tmp" (1024 * 1024)
          state = (initialSessionState budget) { rssBindings = bindings }
      
      Map.lookup "myVar" (rssBindings state) `shouldBe` Just binding

    it "preserves bindings when incrementing execution count" $ do
      let binding = Binding "x" "Int"
          bindings = Map.fromList [("x", binding)]
          budget = ResourceBudget 5 0 "/tmp" (1024 * 1024)
          initialState = (initialSessionState budget) { rssBindings = bindings }
          updatedState = incrementExecutionCount initialState
      
      rssBindings updatedState `shouldBe` bindings
      rssExecutionCount updatedState `shouldBe` 1

  describe "module artifact management" $ do
    it "can store and retrieve module artifacts" $ do
      let artifact = ModuleArtifact "/tmp/module.o" (Just "/tmp/module.hi") "abc123"
          modules = Map.fromList [("MyModule", artifact)]
          budget = ResourceBudget 5 0 "/tmp" (1024 * 1024)
          state = (initialSessionState budget) { rssLoadedModules = modules }
      
      Map.lookup "MyModule" (rssLoadedModules state) `shouldBe` Just artifact

    it "handles modules without interface files" $ do
      let artifact = ModuleArtifact "/tmp/module.o" Nothing "def456"
          modules = Map.fromList [("SimpleModule", artifact)]
          budget = ResourceBudget 5 0 "/tmp" (1024 * 1024)
          state = (initialSessionState budget) { rssLoadedModules = modules }
      
      case Map.lookup "SimpleModule" (rssLoadedModules state) of
        Just a -> do
          maObjectPath a `shouldBe` "/tmp/module.o"
          maInterfacePath a `shouldBe` Nothing
          maHash a `shouldBe` "def456"
        Nothing -> expectationFailure "Module artifact should be found"

  describe "import management" $ do
    it "can store and retrieve imports list" $ do
      let imports = ["Data.List", "Control.Monad", "qualified Data.Map as Map"]
          budget = ResourceBudget 5 0 "/tmp" (1024 * 1024)
          state = (initialSessionState budget) { rssImports = imports }
      
      rssImports state `shouldBe` imports
      length (rssImports state) `shouldBe` 3

    it "preserves imports when incrementing execution count" $ do
      let imports = ["Data.List", "Control.Monad"]
          budget = ResourceBudget 5 0 "/tmp" (1024 * 1024)
          initialState = (initialSessionState budget) { rssImports = imports }
          updatedState = incrementExecutionCount initialState
      
      rssImports updatedState `shouldBe` imports
