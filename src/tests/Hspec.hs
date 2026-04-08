module Main where

import           Prelude
import           Control.Monad (when)
import           Data.List (isPrefixOf)
import           Data.Maybe (fromMaybe, listToMaybe)
import           System.Directory (doesPathExist, getCurrentDirectory, listDirectory)
import           System.Environment (lookupEnv, setEnv)
import           System.FilePath ((</>))

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
  -- Set GHC_ENVIRONMENT so that runGhc sessions (used by completion tests)
  -- can find local packages, regardless of the working directory.
  envFiles <- listDirectory currentDir
  case listToMaybe $ filter (".ghc.environment." `isPrefixOf`) envFiles of
    Just f  -> setEnv "GHC_ENVIRONMENT" (currentDir </> f)
    Nothing -> return ()
  hspec $ do
    testParser
    testEval
    testCompletions
    testHoogle
