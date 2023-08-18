module Main where

import           GHC.IO.Encoding
import           Prelude

import           Test.Hspec

import           IHaskell.Test.Completion (testCompletions)
import           IHaskell.Test.Parser (testParser)
import           IHaskell.Test.Eval (testEval)
import           IHaskell.Test.Hoogle (testHoogle)

main :: IO ()
main = do
  setLocaleEncoding utf8

  hspec $ do
    testParser
    testEval
    testCompletions
    testHoogle
