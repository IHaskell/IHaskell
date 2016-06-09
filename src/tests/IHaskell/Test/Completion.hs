{-# LANGUAGE CPP #-}
module IHaskell.Test.Completion (testCompletions) where

import           Prelude

import           Data.List (elemIndex)
import qualified Data.Text as T
import           Control.Monad.IO.Class (liftIO)
import           System.Environment (setEnv)
import           System.Directory (setCurrentDirectory, getCurrentDirectory)

import           GHC (getSessionDynFlags, setSessionDynFlags, DynFlags(..), GhcLink(..), setContext,
                      parseImportDecl, HscTarget(..), InteractiveImport(..))

import           Test.Hspec

import           Shelly (toTextIgnore, (</>), shelly, fromText, get_env_text, FilePath, cd, mkdir_p,
                         touchfile, withTmpDir)

import           IHaskell.Eval.Evaluate (Interpreter, liftIO)
import           IHaskell.Eval.Completion (complete, CompletionType(..), completionType,
                                           completionTarget)
import           IHaskell.Test.Util (replace, shouldBeAmong, ghc)

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative ((<$>))
#endif

-- | @readCompletePrompt "xs*ys"@ return @(xs, i)@ where i is the location of
-- @'*'@ in the input string. 
readCompletePrompt :: String -> (String, Int)
readCompletePrompt string =
  case elemIndex '*' string of
    Nothing  -> error "Expected cursor written as '*'."
    Just idx -> (replace "*" "" string, idx)

completionEvent :: String -> Interpreter (String, [String])
completionEvent string = complete newString cursorloc
  where
    (newString, cursorloc) =
      case elemIndex '*' string of
        Nothing  -> error "Expected cursor written as '*'."
        Just idx -> (replace "*" "" string, idx)

completionEventInDirectory :: String -> IO (String, [String])
completionEventInDirectory string = withHsDirectory $ const $ completionEvent string

shouldHaveCompletionsInDirectory :: String -> [String] -> IO ()
shouldHaveCompletionsInDirectory string expected = do
  (matched, completions) <- completionEventInDirectory string
  let existsInCompletion = (`elem` completions)
      unmatched = filter (not . existsInCompletion) expected
  expected `shouldBeAmong` completions

completionHas string expected = do
  (matched, completions) <- ghc $ do
                              initCompleter
                              completionEvent string
  let existsInCompletion = (`elem` completions)
      unmatched = filter (not . existsInCompletion) expected
  expected `shouldBeAmong` completions

initCompleter :: Interpreter ()
initCompleter = do
  flags <- getSessionDynFlags
  setSessionDynFlags $ flags { hscTarget = HscInterpreted, ghcLink = LinkInMemory }

  -- Import modules.
  imports <- mapM parseImportDecl
               [ "import Prelude"
               , "import qualified Control.Monad"
               , "import qualified Data.List as List"
               , "import IHaskell.Display"
               , "import Data.Maybe as Maybe"
               ]
  setContext $ map IIDecl imports

completes :: String -> [String] -> IO ()
completes string expected = completionTarget newString cursorloc `shouldBe` expected
  where
    (newString, cursorloc) = readCompletePrompt string

testCompletions :: Spec
testCompletions = do
  testIdentifierCompletion
  testCommandCompletion

testIdentifierCompletion :: Spec
testIdentifierCompletion = describe "Completion" $ do
    it "correctly gets the completion identifier without dots" $ do
      "hello*" `completes` ["hello"]
      "hello aa*bb goodbye" `completes` ["aa"]
      "hello aabb* goodbye" `completes` ["aabb"]
      "aacc* goodbye" `completes` ["aacc"]
      "hello *aabb goodbye" `completes` []
      "*aabb goodbye" `completes` []

    it "correctly gets the completion identifier with dots" $ do
      "hello test.aa*bb goodbye" `completes` ["test", "aa"]
      "Test.*" `completes` ["Test", ""]
      "Test.Thing*" `completes` ["Test", "Thing"]
      "Test.Thing.*" `completes` ["Test", "Thing", ""]
      "Test.Thing.*nope" `completes` ["Test", "Thing", ""]

    it "correctly gets the completion type" $ do
      completionType "import Data." 12 ["Data", ""] `shouldBe` ModuleName "Data" ""
      completionType "import Prel" 11 ["Prel"] `shouldBe` ModuleName "" "Prel"
      completionType "import D.B.M" 12 ["D", "B", "M"] `shouldBe` ModuleName "D.B" "M"
      completionType " import A." 10 ["A", ""] `shouldBe` ModuleName "A" ""
      completionType "import a.x" 10 ["a", "x"] `shouldBe` Identifier "x"
      completionType "A.x" 3 ["A", "x"] `shouldBe` Qualified "A" "x"
      completionType "a.x" 3 ["a", "x"] `shouldBe` Identifier "x"
      completionType "pri" 3 ["pri"] `shouldBe` Identifier "pri"
      completionType ":load A" 7 ["A"] `shouldBe` HsFilePath ":load A" "A"
      completionType ":! cd " 6 [""] `shouldBe` FilePath ":! cd " ""



    it "properly completes identifiers" $ do
      "pri*" `completionHas` ["print"]
      "ma*" `completionHas` ["map"]
      "hello ma*" `completionHas` ["map"]
      "print $ catMa*" `completionHas` ["catMaybes"]

    it "properly completes qualified identifiers" $ do
      "Control.Monad.liftM*" `completionHas` [ "Control.Monad.liftM"
                                             , "Control.Monad.liftM2"
                                             , "Control.Monad.liftM5"
                                             ]
      "print $ List.intercal*" `completionHas` ["List.intercalate"]
      "print $ Data.Maybe.cat*" `completionHas` []
      "print $ Maybe.catM*" `completionHas` ["Maybe.catMaybes"]

    it "properly completes imports" $ do
      "import Data.*" `completionHas` ["Data.Maybe", "Data.List"]
      "import Data.M*" `completionHas` ["Data.Maybe"]
      "import Prel*" `completionHas` ["Prelude"]


testCommandCompletion :: Spec
testCommandCompletion = describe "Completes commands" $ do
  it "properly completes haskell file paths on :load directive" $ do
    let loading xs = ":load " ++ T.unpack (toTextIgnore xs)
        paths = map (T.unpack . toTextIgnore)
        testInDirectory start comps = loading start `shouldHaveCompletionsInDirectory` paths comps
    testInDirectory ("dir" </> "file*") ["dir" </> "file2.hs", "dir" </> "file2.lhs"]
    testInDirectory ("" </> "file1*") ["" </> "file1.hs", "" </> "file1.lhs"]
    testInDirectory ("" </> "file1*") ["" </> "file1.hs", "" </> "file1.lhs"]
    testInDirectory ("" </> "./*") ["./" </> "dir/", "./" </> "file1.hs", "./" </> "file1.lhs"]
    testInDirectory ("" </> "./*") ["./" </> "dir/", "./" </> "file1.hs", "./" </> "file1.lhs"]

  it "provides path completions on empty shell cmds " $
    ":! cd *" `shouldHaveCompletionsInDirectory` map (T.unpack . toTextIgnore)
                                                   [ "" </> "dir/"
                                                   , "" </> "file1.hs"
                                                   , "" </> "file1.lhs"
                                                   ]

  let withHsHome action = withHsDirectory $ \dirPath -> do
        home <- shelly $ Shelly.get_env_text "HOME"
        setHomeEvent dirPath
        result <- action
        setHomeEvent $ Shelly.fromText home
        return result
      setHomeEvent path = liftIO $ setEnv "HOME" (T.unpack $ toTextIgnore path)

  it "correctly interprets ~ as the environment HOME variable" $ do
    let shouldHaveCompletions :: String -> [String] -> IO ()
        shouldHaveCompletions string expected = do
          (matched, completions) <- withHsHome $ completionEvent string
          let existsInCompletion = (`elem` completions)
              unmatched = filter (not . existsInCompletion) expected
          expected `shouldBeAmong` completions
    ":! cd ~/*" `shouldHaveCompletions` ["~/dir/"]
    ":! ~/*" `shouldHaveCompletions` ["~/dir/"]
    ":load ~/*" `shouldHaveCompletions` ["~/dir/"]
    ":l ~/*" `shouldHaveCompletions` ["~/dir/"]

  let shouldHaveMatchingText :: String -> String -> IO ()
      shouldHaveMatchingText string expected = do
        matchText <- withHsHome $ fst <$> uncurry complete (readCompletePrompt string)
        matchText `shouldBe` expected

      setHomeEvent path = liftIO $ setEnv "HOME" (T.unpack $ toTextIgnore path)

  it "generates the correct matchingText on `:! cd ~/*` " $
    ":! cd ~/*" `shouldHaveMatchingText` ("~/" :: String)

  it "generates the correct matchingText on `:load ~/*` " $
    ":load ~/*" `shouldHaveMatchingText` ("~/" :: String)

  it "generates the correct matchingText on `:l ~/*` " $
    ":l ~/*" `shouldHaveMatchingText` ("~/" :: String)

inDirectory :: [Shelly.FilePath] -- ^ directories relative to temporary directory
            -> [Shelly.FilePath] -- ^ files relative to temporary directory
            -> (Shelly.FilePath -> Interpreter a)
            -> IO a
-- | Run an Interpreter action, but first make a temporary directory 
--   with some files and folder and cd to it.
inDirectory dirs files action = shelly $ withTmpDir $ \dirPath -> do
  cd dirPath
  mapM_ mkdir_p dirs
  mapM_ touchfile files
  liftIO $ ghc $ wrap (T.unpack $ toTextIgnore dirPath) (action dirPath)
  where
    cdEvent path = liftIO $ setCurrentDirectory path
    wrap :: String -> Interpreter a -> Interpreter a
    wrap path action = do
      initCompleter
      pwd <- IHaskell.Eval.Evaluate.liftIO getCurrentDirectory
      cdEvent path   -- change to the temporary directory
      out <- action  -- run action
      cdEvent pwd    -- change back to the original directory
      return out

withHsDirectory :: (Shelly.FilePath -> Interpreter a) -> IO a
withHsDirectory = inDirectory [p "" </> p "dir", p "dir" </> p "dir1"]
                    [ p "" </> p "file1.hs"
                    , p "dir" </> p "file2.hs"
                    , p "" </> p "file1.lhs"
                    , p "dir" </> p "file2.lhs"
                    ]
  where
    p = id
