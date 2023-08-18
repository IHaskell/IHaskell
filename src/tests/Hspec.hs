module Main where

import           Prelude

import           Test.Hspec

import           IHaskell.Test.Completion (testCompletions)
import           IHaskell.Test.Parser (testParser)
import           IHaskell.Test.Eval (testEval)
import           IHaskell.Test.Hoogle (testHoogle)

main :: IO ()
main =
  hspec $ do
    testParser
    testEval
    testCompletions
    testHoogle
