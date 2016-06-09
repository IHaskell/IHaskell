module Main where

import           Prelude

import           Test.Hspec

import           IHaskell.Test.Completion (testCompletions)
import           IHaskell.Test.Parser (testParser)
import           IHaskell.Test.Eval (testEval)

main :: IO ()
main = 
  hspec $ do
    testParser
    testEval
    testCompletions
