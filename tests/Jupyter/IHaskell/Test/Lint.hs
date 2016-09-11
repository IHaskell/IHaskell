{-|
Module      : Jupyter.IHaskell.Test.Lint
Description : Tests for Jupyter.IHaskell.Lint
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings #-}
module Jupyter.IHaskell.Test.Lint (lintTests) where

-- Imports from 'base'
import           Control.Monad (unless)

-- Imports from 'tasty'
import           Test.Tasty (TestTree)

-- Imports from 'tasty-hunit'
import           Test.Tasty.HUnit (testCase, assertFailure)

-- Imports from 'transformers'
import           Control.Monad.IO.Class (MonadIO(..))

-- Imports from 'ihaskell'
import           Jupyter.IHaskell.Lint (lintCodeBlock, LintSuggestion(..), SuggestionType(..))
import           Jupyter.IHaskell.Parser (Loc(..), CodeBlock(..))

-- | Test the IHaskell HLint integration.
lintTests :: TestTree
lintTests = testCase "Linting" $ do
  -- Basic expression linting.
  Expression "(f x)" --> [ LintSuggestion
                           { lintSuggestionLine = 1
                           , lintSuggestionFound = "(f x)"
                           , lintSuggestionWhyNot = "f x"
                           , lintSuggestionType = InfoSuggestion
                           , lintSuggestion = "Redundant bracket"
                           }
                         ]
  Expression "f (f $ x)" --> [ LintSuggestion
                               { lintSuggestionLine = 1
                               , lintSuggestionFound = "f $ x"
                               , lintSuggestionWhyNot = "f x"
                               , lintSuggestionType = InfoSuggestion
                               , lintSuggestion = "Redundant $"
                               }
                             ]

  -- Basic declaration linting.
  Declarations "x = f (f $ x)" --> [ LintSuggestion
                                     { lintSuggestionLine = 1
                                     , lintSuggestionFound = "f $ x"
                                     , lintSuggestionWhyNot = "f x"
                                     , lintSuggestionType = InfoSuggestion
                                     , lintSuggestion = "Redundant $"
                                     }
                                   ]

  -- Linting multiple declarations at once.
  Declarations "x = f (f $ x)\nx = f (f $ x)" --> [ LintSuggestion
                                                    { lintSuggestionLine = 1
                                                    , lintSuggestionFound = "f $ x"
                                                    , lintSuggestionWhyNot = "f x"
                                                    , lintSuggestionType = InfoSuggestion
                                                    , lintSuggestion = "Redundant $"
                                                    }
                                                  , LintSuggestion
                                                    { lintSuggestionLine = 2
                                                    , lintSuggestionFound = "f $ x"
                                                    , lintSuggestionWhyNot = "f x"
                                                    , lintSuggestionType = InfoSuggestion
                                                    , lintSuggestion = "Redundant $"
                                                    }
                                                  ]
  Statement "x <- f (f $ x)" --> [ LintSuggestion
                                   { lintSuggestionLine = 1
                                   , lintSuggestionFound = "f $ x"
                                   , lintSuggestionWhyNot = "f x"
                                   , lintSuggestionType = InfoSuggestion
                                   , lintSuggestion = "Redundant $"
                                   }
                                 ]

-- | Run a single linter test. Lint the input and then compare output suggestions to generated
-- suggestions.
(-->) :: CodeBlock -> [LintSuggestion] -> IO ()
block --> expected = do
  observed <- lintCodeBlock (Loc 1 block)
  unless (and (zipWith (==) expected observed) && length expected == length observed) $
    liftIO $ assertFailure $
      concat
        [ "Did not get expected suggestions for: "
        , show block
        , "\nExpected: "
        , show expected
        , "\nObserved: "
        , show observed
        ]
