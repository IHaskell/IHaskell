module Main where

import           Prelude
import           Control.Monad (when)
import           System.Directory (doesPathExist, getCurrentDirectory)
import           System.Environment (lookupEnv, setEnv)
import           Data.Maybe (fromMaybe)

import           Test.Hspec

import           IHaskell.Test.Completion (testCompletions)
import           IHaskell.Test.Parser (testParser)
import           IHaskell.Test.Eval (testEval)
import           IHaskell.Test.Hoogle (testHoogle)

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  packageConfInPlaceExists <- doesPathExist (currentDir ++ "/dist/package.conf.inplace")
  when packageConfInPlaceExists $ do
    ghcPackagePath <- fromMaybe "" <$> lookupEnv "GHC_PACKAGE_PATH"
    setEnv "GHC_PACKAGE_PATH" $ currentDir ++ "/dist/package.conf.inplace/" ++ ":" ++ ghcPackagePath
  hspec $ do
    testParser
    testEval
    testCompletions
    testHoogle
