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
import           System.Environment (getEnv, setEnv)
import           Control.Exception (bracket)


-- Imports from 'text'
import           Data.Text (Text)
import qualified Data.Text as T

-- Imports from 'directory'
import           System.Directory (createDirectoryIfMissing)

-- Imports from 'extra'
import           System.IO.Extra (withTempDir)
import           System.Directory.Extra (withCurrentDirectory)

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
import           Jupyter.IHaskell.Evaluate (evalImport)
import           Jupyter.IHaskell.Interpreter (runInterpreter, Interpreter)

completionTests :: TestTree
completionTests =
  testGroup "Completion"
    [ importCompletionTests
    , flagCompletionTests
    , identifierCompletionTests
    , pathCompletionTests
    , sourceCompletionTests
    ]

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

flagCompletionTests :: TestTree
flagCompletionTests = testCase "Flags" $ void $ runInterpreter (Just libdir) $ do
  ":set -XNoMonom" --> [Completion 9 "-XNoMonomorphismRestriction"]
  ":set -XMono" --> [Completion 6 "-XMonoLocalBinds"]
  ":set -XMonom" --> [Completion 7 "-XMonomorphismRestriction"]
  ":set -XGAD" --> [Completion 5 "-XGADTSyntax", Completion 5 "-XGADTs"]
  ":set -pack" --> [Completion 5 "-package"]
  ":set -Wa" --> [Completion 3 "-Wall"]
  ":set -fallow-undeci" --> [Completion 14 "-fallow-undecidable-instances"]
  ":set -fnoallow-undeci" --> [Completion 16 "-fnoallow-undecidable-instances"]

  nothing ":set -XASDF"

identifierCompletionTests :: TestTree
identifierCompletionTests = testCase "Identifiers" $ void $ runInterpreter (Just libdir) $ do
  -- Completions from Prelude
  evalImport "import Prelude"
  "let x = pri" --> [Completion 3 "print"]
  "let x =pri" --> [Completion 3 "print"]
  "let x=pri" --> [Completion 3 "print"]
  "print (putStr" --> [Completion 6 "putStr", Completion 6 "putStrLn"]
  "print(putStr" --> [Completion 6 "putStr", Completion 6 "putStrLn"]
  "hGetCon <> mcon" --> [Completion 4 "mconcat"]
  "hGetCon <>mcon" --> [Completion 4 "mconcat"]
  "hGetCon<>mcon" --> [Completion 4 "mconcat"]
  "x :: Inte" --> [Completion 4 "Integer", Completion 4 "Integral"]
  "x ::Inte" --> [Completion 4 "Integer", Completion 4 "Integral"]
  "x::Inte" --> [Completion 4 "Integer", Completion 4 "Integral"]
  "x:: Mono" --> [Completion 4 "Monoid"]
  "x:: Mona" --> [Completion 4 "Monad"]
  "x::Mona" --> [Completion 4 "Monad"]
  "x:: x =>Inte" --> [Completion 4 "Integer", Completion 4 "Integral"]
  "x:: x ->Inte" --> [Completion 4 "Integer", Completion 4 "Integral"]
  "x:: x ->Inte" --> [Completion 4 "Integer", Completion 4 "Integral"]
  "data X = X { y :: !Inte" --> [Completion 4 "Integer", Completion 4 "Integral"]
  "data X = X { y ::!Inte" --> [Completion 4 "Integer", Completion 4 "Integral"]
  "Prelude.put" --> [Completion 3 "putStr", Completion 3 "putChar", Completion 3 "putStrLn"]
  "System.Env" --> [Completion 10 "System.Environment"]
  "System.Environment.getA" --> [Completion 4 "getArgs"]

  nothing "1 + hGetCon"
  nothing "1 +hGetCon"
  nothing "1+hGetCon"

  evalImport "import System.IO"
  "1 + hGetCon" --> [Completion 7 "hGetContents"]
  "1 +hGetCon" --> [Completion 7 "hGetContents"]
  "1+hGetCon" --> [Completion 7 "hGetContents"]
  "x::Inte" --> [Completion 4 "Integer", Completion 4 "Integral"]

pathCompletionTests :: TestTree
pathCompletionTests = testCase "Paths" $ inTempDir $ \tmp ->
  bracket (getEnv "HOME") (setEnv "HOME") $ const $ do
    -- Set up some paths to use for testing.
    setEnv "HOME" tmp
    createDirectoryIfMissing True "test-direc/tory.with-a.weird/name-for"
    createDirectoryIfMissing True "test-direc/tory.number2"
    writeFile "test-direc/tory.number2/filename" "test"
    writeFile "test-direc/filename" "test"
    writeFile "test-direc/.filename" "test"
    writeFile "test-direc/tory.with-a.weird/filename" "test"
    writeFile "test-direc/tory.with-a.weird/name-for/filename" "test"

    void $ runInterpreter (Just libdir) $ do
      -- Paths with ~ in them
      "readFile \"Hello, ~/test" --> [Completion 6 "~/test-direc/"]
      "readFile \"Hello, ~/" --> [Completion 2 "~/test-direc/"]
      "readFile \"Hello, ~/test-direc/tory.number2/" -->
        [Completion 26 "~/test-direc/tory.number2/filename"]
      "readFile \"Hello, ~/test-direc/" -->
        [ Completion 13 "~/test-direc/tory.number2/"
        , Completion 13 "~/test-direc/tory.with-a.weird/"
        , Completion 13 "~/test-direc/filename"
        , Completion 13 "~/test-direc/.filename"
        ]
      "readFile \"He\"ll\"o, ~/test-direc/tory.number2/" -->
        [Completion 26 "~/test-direc/tory.number2/filename"]
      "readFile \"He\"ll\"o\", \"~/test-direc/tory.number2/" -->
        [ Completion 26 "~/test-direc/tory.number2/filename" ]
      nothing "readFile \"Hello\" ~/test-direc/tory.number2/"
      nothing "readFile \"Hel\" \"lo\" ~/test-direc/tory.number2/"

      -- Paths with ./ in them
      "readFile \"Hello, ./test" --> [Completion 6 "./test-direc/"]
      "readFile \"Hello, ./" --> [Completion 2 "./test-direc/"]
      "readFile \"Hello, ./test-direc/tory.number2/" -->
        [Completion 26 "./test-direc/tory.number2/filename"]
      "readFile \"Hello, ./test-direc/" -->
        [ Completion 13 "./test-direc/tory.number2/"
        , Completion 13 "./test-direc/tory.with-a.weird/"
        , Completion 13 "./test-direc/filename"
        , Completion 13 "./test-direc/.filename"
        ]
      "readFile \"He\"ll\"o, ./test-direc/tory.number2/" -->
        [Completion 26 "./test-direc/tory.number2/filename"]
      "readFile \"He\"ll\"o\", \"./test-direc/tory.number2/" -->
        [ Completion 26 "./test-direc/tory.number2/filename" ]
      nothing "readFile \"Hello\" ./test-direc/tory.number2/"
      nothing "readFile \"Hel\" \"lo\" ./test-direc/tory.number2/"

      -- Paths relative to current directory
      "readFile \"Hello, test" --> [Completion 4 "test-direc/"]
      "readFile \"Hello, test-direc/tory.number2/" -->
        [Completion 24 "test-direc/tory.number2/filename"]
      "readFile \"Hello, test-direc/" -->
        [ Completion 11 "test-direc/tory.number2/"
        , Completion 11 "test-direc/tory.with-a.weird/"
        , Completion 11 "test-direc/filename"
        , Completion 11 "test-direc/.filename"
        ]
      "readFile \"He\"ll\"o, test-direc/tory.number2/" -->
        [Completion 24 "test-direc/tory.number2/filename"]
      "readFile \"He\"ll\"o\", \"test-direc/tory.number2/" -->
        [ Completion 24 "test-direc/tory.number2/filename" ]
      nothing "readFile \"Hello, "
      nothing "readFile \"Hello\" test-direc/tory.number2/"
      nothing "readFile \"Hel\" \"lo\" test-direc/tory.number2/"

      -- Absolute paths
      let absolutize = T.replace "{}" (T.pack tmp)
          len = length tmp
      absolutize "readFile \"Hello, {}/test" --> [Completion (len + 5) $ absolutize "{}/test-direc/"]
      absolutize "readFile \"Hello, {}/test-direc/tory.number2/" -->
        [Completion (len + 25) $ absolutize "{}/test-direc/tory.number2/filename"]
      absolutize "readFile \"Hello, {}/test-direc/" -->
        [ Completion (len + 12) $ absolutize "{}/test-direc/tory.number2/"
        , Completion (len + 12) $ absolutize "{}/test-direc/tory.with-a.weird/"
        , Completion (len + 12) $ absolutize "{}/test-direc/filename"
        , Completion (len + 12) $ absolutize "{}/test-direc/.filename"
        ]
      absolutize "readFile \"He\"ll\"o, {}/test-direc/tory.number2/" -->
        [Completion (len + 25) $ absolutize "{}/test-direc/tory.number2/filename"]
      absolutize "readFile \"He\"ll\"o\", \"{}/test-direc/tory.number2/" -->
        [ Completion (len + 25) $ absolutize "{}/test-direc/tory.number2/filename" ]

      nothing $ absolutize "readFile \"Hello\" {}/test-direc/tory.number2/"
      nothing $ absolutize "readFile \"Hel\" \"lo\" {}/test-direc/tory.number2/"

sourceCompletionTests :: TestTree
sourceCompletionTests = testCase "Source" $ inTempDir $ \tmp -> do
  createDirectoryIfMissing True "test-direc/tory.number2"
  writeFile "test-direc/test.test" "test"
  writeFile "test-direc/test.hs" "test"
  writeFile "test-direc/test.lhs" "test"
  writeFile "test-direc/tory.number2/test.test" "test"
  writeFile "test-direc/tory.number2/test.hs" "test"
  writeFile "test-direc/tory.number2/test.lhs" "test"

  void $ runInterpreter (Just libdir) $ do
    ":l ./test-dir" --> [Completion 10 "./test-direc/"]
    ":load ./test-dir" --> [Completion 10 "./test-direc/"]
    ":l ./test-direc/" --> [ Completion 13 "./test-direc/tory.number2/"
                           , Completion 13 "./test-direc/test.hs"
                           , Completion 13 "./test-direc/test.lhs"
                           ]
    ":load ./test-direc/" --> [ Completion 13 "./test-direc/tory.number2/"
                              , Completion 13 "./test-direc/test.hs"
                              , Completion 13 "./test-direc/test.lhs"
                              ]
    ":l ./test-direc/tory.number2/" --> [ Completion 26 "./test-direc/tory.number2/test.hs"
                                        , Completion 26 "./test-direc/tory.number2/test.lhs"
                                        ]
    ":load ./test-direc/tory.number2/" --> [ Completion 26 "./test-direc/tory.number2/test.hs"
                                           , Completion 26 "./test-direc/tory.number2/test.lhs"
                                           ]



-- | Assert that completions for a particular starting string begin with a particular set of
-- completions.
(-->) :: Text -> [Completion] -> Interpreter ()
text --> expected = do
  observed <- complete text
  when (null observed) $
    liftIO $ assertFailure $ "No completions for: " ++ show text
  unless (and (zipWith (==) expected observed) && length expected <= length observed) $
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

-- | Create a temporary directory and execute an action with that temporary directory as the working
-- directory. This is not threadsafe, since working directories are global values.
inTempDir :: (FilePath -> IO a) -> IO a
inTempDir action = withTempDir $ \tmp -> withCurrentDirectory tmp (action tmp)
