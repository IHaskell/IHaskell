{-|
Module      : Jupyter.Test.ZeroMQ
Description : Miscellaneous tests for Jupyter.ZeroMQ. 
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
module Jupyter.IHaskell.Test.Complete (completionTests) where

-- Imports from 'base'
import           Control.Monad (when, unless, void)

-- Imports from 'text'
import           Data.Text (Text)

-- Imports from 'tasty'
import           Test.Tasty (TestTree, testGroup)

-- Imports from 'tasty-hunit'
import           Test.Tasty.HUnit (testCase, (@=?), assertFailure)

-- Imports from 'transformers'
import           Control.Monad.IO.Class (MonadIO(..))

-- Imports from 'ghc'
import           GHC.Paths (libdir)

-- Imports from 'ihaskell'
import           Jupyter.IHaskell.Complete (complete, Completion(..))
import           Jupyter.IHaskell.Interpreter (runInterpreter, Interpreter)

completionTests :: TestTree
completionTests = testGroup "Completion" [importCompletionTests, extensionCompletionTests, flagCompletionTests]

importCompletionTests :: TestTree
importCompletionTests = testCase "Imports" $ void $ runInterpreter (Just libdir) $ do
  "import qu" --> [Completion 2 "qualified"]
  "import qualif" --> [Completion 6 "qualified"]
  "import Dat" --> [Completion 3 "Data."]
  "import Con" --> [Completion 3 "Control.", Completion 3 "Control.Arrow", Completion 3 "Control.Monad"]
  "import Control.Con" --> [Completion 11 "Control.Concurrent", Completion 11 "Control.Concurrent.STM"]
  "import Data." --> [Completion 5 "Data.Eq", Completion 5 "Data.Ix",  Completion 5 "Data.Map"]
  "import Data.Mo" --> [Completion 7 "Data.Monoid"]
  "import     Data.Mono" --> [Completion 9 "Data.Monoid"]
  "import Data.Monoid" --> [Completion 11 "Data.Monoid"]
  "import qualified Data." --> [Completion 5 "Data.Eq", Completion 5 "Data.Ix", Completion 5 "Data.Map"]
  "import qualified Data.Mo" --> [Completion 7 "Data.Monoid"]
  "import qualified     Data.Mono" --> [Completion 9 "Data.Monoid"]
  "import       qualified Data.Monoid" --> [Completion 11 "Data.Monoid"]
  "import Unsafe." --> [Completion 7 "Unsafe.Coerce"]
  "import Unsafe.Co" --> [Completion 9 "Unsafe.Coerce"]
  "import Data.ByteString.L" --> [Completion 17 "Data.ByteString.Lazy"]
  "import Data.Monoid (m" --> [Completion 1 "mempty", Completion 1 "mappend", Completion 1 "mconcat"]
  "import Data.Monoid (mc" --> [Completion 2 "mconcat"]
  "import Data.Monoid   (me" --> [Completion 2 "mempty"]
  "import Data.Monoid (mempty, getD" --> [Completion 4 "getDual"]
  "import Data.Monoid (mc Pro" --> [Completion 3 "Product"]
  "import Data.Monoid (mc  Pro" --> [Completion 3 "Product"]

  nothing "import Data.Monoid (mc  Pro)"
  nothing "import Data.Monoid (a, b) "
  nothing "import Data.DoesntExist (abc"

extensionCompletionTests :: TestTree
extensionCompletionTests = testCase "Extensions" $ void $ runInterpreter (Just libdir) $ do
  ":set -XNoMonom" --> [Completion 9 "-XNoMonomorphismRestriction"]
  ":set -XMono" --> [Completion 6 "-XMonoLocalBinds"]
  ":ext NoMonom" --> [Completion 7 "NoMonomorphismRestriction"]
  ":ext Monom" --> [Completion 5 "MonomorphismRestriction"]
  ":ext GAD" --> [Completion 3 "GADTSyntax", Completion 3 "GADTs"]
  ":e NoMonom" --> [Completion 7 "NoMonomorphismRestriction"]
  ":e Monom" --> [Completion 5 "MonomorphismRestriction"]
  ":e GAD" --> [Completion 3 "GADTSyntax", Completion 3 "GADTs"]

  nothing ":ext -XGAD"
  nothing ":set -XASDF"
  nothing ":ext -XASDF"
  nothing ":ext ASDF"

flagCompletionTests :: TestTree
flagCompletionTests = testCase "Flags" $ void $ runInterpreter (Just libdir) $ do
  ":set -pack" --> [Completion 5 "-package"]
  ":set -Wa" --> [Completion 3 "-Wall"]
  ":set -fallow-undeci" --> [Completion 14 "-fallow-undecidable-instances"]
  ":set -fnoallow-undeci" --> [Completion 16 "-fnoallow-undecidable-instances"]

-- | Assert that completions for a particular starting string begin with a particular set of
-- completions.
(-->) :: Text -> [Completion] -> Interpreter ()
text --> expected = do
  observed <- complete text
  when (null observed) $
    liftIO $ assertFailure $ "No completions for: " ++ show text
  unless (and $ zipWith (==) expected observed) $
    liftIO $ assertFailure $
      concat
        [ "Did not get expected completions for: "
        , show text
        , "\nExpected: "
        , show expected
        , "\nObserved: "
        , show observed
        ]

-- | Assert that there are no completions for a particular starting string.
nothing :: Text -> Interpreter ()
nothing text = do
  observed <- complete text
  unless (null observed) $
    liftIO $ assertFailure $ "Expected no completions for: " ++ show text
