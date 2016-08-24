module Main (main) where

-- Imports from 'tasty'
import           Test.Tasty (defaultMain, testGroup)

-- Imports from 'jupyter'
import           Jupyter.IHaskell.Test.Complete (completionTests)

-- | Run all Haskell tests for the @ihaskell@ library package.
main :: IO ()
main =
  defaultMain $
    testGroup "Tests" [completionTests]
