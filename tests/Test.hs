module Main (main) where

-- Imports from 'tasty'
import           Test.Tasty (defaultMain, testGroup)

-- Imports from 'ihaskell'
import           Jupyter.IHaskell.Test.Complete (completionTests)
import           Jupyter.IHaskell.Test.Lint (lintTests)
import           Jupyter.IHaskell.Test.Parser (parserTests)

-- | Run all Haskell tests for the @ihaskell@ library package.
main :: IO ()
main =
  defaultMain $
    testGroup "Tests" [parserTests, completionTests, lintTests]
