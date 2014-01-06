{-# LANGUAGE QuasiQuotes #-}
module Main where
import Prelude
import GHC
import GHC.Paths
import Data.IORef
import Control.Monad
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.List
import System.Directory
import Shelly (Sh, shelly, cmd, (</>), toTextIgnore, cd, withTmpDir)
import Filesystem.Path.CurrentOS (encodeString)
import Data.String.Here
import Data.String.Utils (strip, replace)
import Data.Monoid

import IHaskell.Eval.Parser
import IHaskell.Types
import IHaskell.IPython
import IHaskell.Eval.Evaluate as Eval hiding (liftIO)
import IHaskell.Eval.Completion

import Test.Hspec
import Test.Hspec.HUnit

doGhc = runGhc (Just libdir)

parses str = do
  res <- doGhc $ parseString str
  return $ map unloc res

like parser desired = parser >>= (`shouldBe` desired)

is string blockType = do
  result <- doGhc $ parseString string
  map unloc result `shouldBe` [blockType $ strip string]

eval string = do
  outputAccum <- newIORef []
  let publish final displayDatas = when final $ modifyIORef outputAccum (displayDatas :)
  getTemporaryDirectory >>= setCurrentDirectory
  let state = defaultKernelState { getLintStatus = LintOff }
  interpret $ Eval.evaluate state string publish
  out <- readIORef outputAccum
  return $ reverse out

becomes string expected = do
    let indent (' ':x) = 1 + indent x
        indent _ = 0
        empty = null . strip
        stringLines = filter (not . empty) $ lines string
        minIndent = minimum (map indent stringLines)
        newString = unlines $ map (drop minIndent) stringLines
    eval newString >>= comparison
  where
    comparison results = do
      when (length results /= length expected) $
        expectationFailure $ "Expected result to have " ++ show (length expected)
                             ++ " results. Got " ++ show results

      let isPlain (Display PlainText _) = True
          isPlain _ = False

      forM_ (zip results expected) $ \(result, expected) ->
        case find isPlain result of
          Just (Display PlainText str) -> str `shouldBe` expected
          Nothing -> expectationFailure $ "No plain-text output in " ++ show result

completes string expected = completionTarget newString cursorloc `shouldBe` expected
  where (newString, cursorloc) = case elemIndex '!' string of
          Nothing -> error "Expected cursor written as '!'."
          Just idx -> (replace "!" "" string, idx)

completionHas_ action string expected = do
    (matched, completions) <- doGhc $ do
      initCompleter action
      complete newString cursorloc
    let existsInCompletion = (`elem` completions)
        unmatched = filter (not . existsInCompletion) expected
    unmatched `shouldBe` []
  where (newString, cursorloc) = case elemIndex '!' string of
          Nothing -> error "Expected cursor written as '!'."
          Just idx -> (replace "!" "" string, idx)

completionHas = completionHas_ (return ())

initCompleter :: GhcMonad m => m a -> m a
initCompleter action = do
  flags <- getSessionDynFlags
  setSessionDynFlags $ flags { hscTarget = HscInterpreted, ghcLink = LinkInMemory }

  -- Import modules.
  imports <- mapM parseImportDecl ["import Prelude",
                                   "import qualified Control.Monad",
                                   "import qualified Data.List as List",
                                   "import Data.Maybe as Maybe"]
  setContext $ map IIDecl imports
  action

withHsDirectory :: (FilePath -> Sh ()) -> IO ()
withHsDirectory f = shelly $ withTmpDir $ \dirPath ->
      do cd dirPath
         cmd "mkdir"  $ "" </> "dir"
         cmd "mkdir"  $ "dir" </> "dir1"
         cmd "touch" "file1.hs"  "dir/file2.hs" "file1.lhs" "dir/file2.lhs"
         f $ encodeString dirPath

main :: IO ()
main = hspec $ do
  parserTests
  evalTests
  completionTests

completionTests = do
  describe "Completion" $ do
    it "correctly gets the completion identifier without dots" $ do
       "hello!"              `completes` ["hello"]
       "hello aa!bb goodbye" `completes` ["aa"]
       "hello aabb! goodbye" `completes` ["aabb"]
       "aacc! goodbye"       `completes` ["aacc"]
       "hello !aabb goodbye" `completes` []
       "!aabb goodbye"       `completes` []

    it "correctly gets the completion identifier with dots" $ do
       "hello test.aa!bb goodbye" `completes` ["test", "aa"]
       "Test.!"                   `completes` ["Test", ""]
       "Test.Thing!"              `completes` ["Test", "Thing"]
       "Test.Thing.!"             `completes` ["Test", "Thing", ""]
       "Test.Thing.!nope"         `completes` ["Test", "Thing", ""]

    it "correctly gets the completion type" $ do
      completionType "import Data." ["Data", ""]      `shouldBe` ModuleName "Data" ""
      completionType "import Prel" ["Prel"]           `shouldBe` ModuleName "" "Prel"
      completionType "import D.B.M" ["D", "B", "M"]   `shouldBe` ModuleName "D.B" "M"
      completionType " import A." ["A", ""]           `shouldBe` ModuleName "A" ""
      completionType "import a.x" ["a", "x"]          `shouldBe` Identifier "x"
      completionType "A.x" ["A", "x"]                 `shouldBe` Qualified "A" "x"
      completionType "a.x" ["a", "x"]                 `shouldBe` Identifier "x"
      completionType "pri" ["pri"]                    `shouldBe` Identifier "pri"
      completionType ":load A" [""]                   `shouldBe` HsFilePath "A"

    it "properly completes identifiers" $ do
       "pri!"           `completionHas` ["print"]
       "ma!"            `completionHas` ["map"]
       "hello ma!"      `completionHas` ["map"]
       "print $ catMa!" `completionHas` ["catMaybes"]

    it "properly completes qualified identifiers" $ do
       "Control.Monad.liftM!"    `completionHas` [ "Control.Monad.liftM"
                                                 , "Control.Monad.liftM2"
                                                 , "Control.Monad.liftM5"]
       "print $ List.intercal!"  `completionHas` ["List.intercalate"]
       "print $ Data.Maybe.cat!" `completionHas` ["Data.Maybe.catMaybes"]
       "print $ Maybe.catM!"     `completionHas` ["Maybe.catMaybes"]

    it "properly completes imports" $ do
      "import Data.!"  `completionHas` ["Data.Maybe", "Data.List"]
      "import Data.M!" `completionHas` ["Data.Maybe"]
      "import Prel!"   `completionHas` ["Prelude"]

    it "properly completes haskell file paths on :load directive" $
       withHsDirectory $ \dirPath ->
         let loading xs = ":load " ++ encodeString xs
             paths xs = map encodeString xs
             completionHas' = completionHas_ $ Eval.evaluate defaultKernelState
                                                             (":! cd " ++ dirPath)
                                                             (\b d -> return ())
         in liftIO $ do
            loading ("dir" </> "file!") `completionHas'` paths ["dir" </> "file2.hs",
                                                            "dir" </> "file2.lhs"]
            loading ("" </> "file1!") `completionHas'` paths ["" </> "file1.hs",
                                                      "" </> "file1.lhs"]

evalTests = do
  describe "Code Evaluation" $ do
    it "evaluates expressions" $  do
      "3" `becomes` ["3"]
      "3+5" `becomes` ["8"]
      "print 3" `becomes` ["3"]
      [hereLit|
        let x = 11
            z = 10 in
          x+z
      |] `becomes` ["21"]

    it "evaluates multiline expressions" $  do
      [hereLit|
        import Control.Monad
        forM_ [1, 2, 3] $ \x ->
          print x
      |] `becomes` ["1\n2\n3"]

    it "evaluates function declarations silently" $ do
      [hereLit|
        fun :: [Int] -> Int
        fun [] = 3
        fun (x:xs) = 10
        fun [1, 2]
      |] `becomes` ["10"]

    it "evaluates data declarations" $ do
      [hereLit|
        data X = Y Int
               | Z String
               deriving (Show, Eq)
        print [Y 3, Z "No"]
        print (Y 3 == Z "No")
      |] `becomes` ["[Y 3,Z \"No\"]", "False"]

    it "evaluates do blocks in expressions" $ do
      [hereLit|
        show (show (do
            Just 10
            Nothing
            Just 100))
      |] `becomes` ["\"\\\"Nothing\\\"\""]

    it "is silent for imports" $ do
      "import Control.Monad" `becomes` []
      "import qualified Control.Monad" `becomes` []
      "import qualified Control.Monad as CM" `becomes` []
      "import Control.Monad (when)" `becomes` []

    it "evaluates directives" $ do
      ":typ 3" `becomes` ["forall a. Num a => a"]
      ":in String" `becomes` ["type String = [Char] \t-- Defined in `GHC.Base'"]

parserTests = do
  layoutChunkerTests
  moduleNameTests
  parseStringTests

layoutChunkerTests = describe "Layout Chunk" $ do
  it "chunks 'a string'" $
    map unloc (layoutChunks "a string") `shouldBe` ["a string"]

  it "chunks 'a\\n string'" $
    map unloc (layoutChunks "a\n string") `shouldBe` ["a\n string"]

  it "chunks 'a\\n string\\nextra'" $
    map unloc (layoutChunks "a\n string\nextra") `shouldBe` ["a\n string","extra"]

  it "chunks strings with too many lines" $
    map unloc (layoutChunks "a\n\nstring") `shouldBe` ["a","string"]

  it "parses multiple exprs" $ do
    let text = [hereLit|
                 first

                 second
                 third

                 fourth
               |]
    layoutChunks text `shouldBe`
      [Located 2 "first",
       Located 4 "second",
       Located 5 "third",
       Located 7 "fourth"]

moduleNameTests = describe "Get Module Name" $ do
  it "parses simple module names" $
    "module A where\nx = 3" `named` ["A"]
  it "parses module names with dots" $
    "module A.B where\nx = 3" `named` ["A", "B"]
  it "parses module names with exports" $
    "module A.B.C ( x ) where x = 3" `named` ["A", "B", "C"]
  it "errors when given unnamed modules" $ do
    doGhc (getModuleName "x = 3") `shouldThrow` anyException
  where
    named str result = do
      res <- doGhc $ getModuleName str
      res `shouldBe` result

parseStringTests = describe "Parser" $ do
  it "parses empty strings" $
    parses "" `like` []

  it "parses simple imports" $
    "import Data.Monoid" `is` Import

  it "parses simple arithmetic" $
    "3 + 5" `is` Expression

  it "parses :type" $
    parses ":type x\n:ty x" `like` [
      Directive GetType "x",
      Directive GetType "x"
    ]

  it "parses :info" $
    parses ":info x\n:in x" `like` [
      Directive GetInfo "x",
      Directive GetInfo "x"
    ]

  it "parses :help and :?" $
    parses ":? x\n:help x" `like` [
      Directive GetHelp "x",
      Directive GetHelp "x"
    ]

  it "parses :set x" $
    parses ":set x" `like` [
      Directive SetOpt "x"
    ]

  it "parses :extension x" $
    parses ":ex x\n:extension x" `like` [
      Directive SetExtension "x",
      Directive SetExtension "x"
    ]

  it "fails to parse :nope" $
    parses ":nope goodbye" `like` [
      ParseError (Loc 1 1) "Unknown directive: 'nope'."
    ]

  it "parses number followed by let stmt" $
    parses "3\nlet x = expr"  `like` [
      Expression "3",
      Statement "let x = expr"
    ]

  it "parses let x in y" $
    "let x = 3 in x + 3" `is` Expression

  it "parses a data declaration" $
    "data X = Y Int" `is` Declaration

  it "parses number followed by type directive" $
    parses "3\n:t expr" `like` [
      Expression "3",
      Directive GetType "expr"
    ]

  it "parses a <- statement" $
    "y <- print 'no'" `is` Statement

  it "parses a <- stmt followed by let stmt" $
    parses "y <- do print 'no'\nlet x = expr" `like` [
      Statement "y <- do print 'no'",
      Statement "let x = expr"
    ]

  it "parses <- followed by let followed by expr" $
    parses "y <- do print 'no'\nlet x = expr\nexpression" `like` [
      Statement "y <- do print 'no'",
      Statement "let x = expr",
      Expression "expression"
    ]

  it "parses two print statements" $
    parses "print yes\nprint no" `like` [
      Expression "print yes",
      Expression "print no"
    ]

  it "parses a pattern-maching function declaration" $
    "fun [] = 10" `is` Declaration

  it "parses a function decl followed by an expression" $
    parses "fun [] = 10\nprint 'h'" `like` [
      Declaration "fun [] = 10",
      Expression "print 'h'"
    ]

  it "parses list pattern matching fun decl" $
    "fun (x : xs) = 100" `is` Declaration

  it "parses two pattern matches as the same declaration" $
    "fun [] = 10\nfun (x : xs) = 100" `is` Declaration

  it "parses a type signature followed by a declaration" $
    "fun :: [a] -> Int\nfun [] = 10\nfun (x : xs) = 100" `is` Declaration

  it "parases a simple module" $
    "module A where x = 3" `is` Module

  it "parses a module with an export" $
    "module B (x) where x = 3" `is` Module

  it "breaks when a let is incomplete" $
    parses "let x = 3 in" `like` [
      ParseError (Loc 1 13) "parse error (possibly incorrect indentation or mismatched brackets)"
    ]

  it "breaks without data kinds" $
    parses "data X = 3" `like` [
      ParseError (Loc 1 10) "Illegal literal in type (use -XDataKinds to enable): 3"
    ]

  it "parses statements after imports" $ do
    parses "import X\nprint 3" `like` [
        Import "import X",
        Expression "print 3"
      ]
    parses "import X\n\nprint 3" `like` [
        Import "import X",
        Expression "print 3"
      ]
  it "ignores blank lines properly" $
    [hereLit|
      test arg = hello
        where
          x = y

          z = w
    |] `is` Declaration
  it "doesn't break on long strings" $ do
    let longString = concat $ replicate 20 "hello "
    ("img ! src \"" ++ longString ++ "\" ! width \"500\"") `is` Expression

  it "parses do blocks in expression" $ do
    [hereLit|
      show (show (do
        Just 10
        Nothing
        Just 100))
    |] `is` Expression
  it "correctly locates parsed items" $ do
    let go = doGhc . parseString
    go [hereLit|
        first

        second
       |] >>= (`shouldBe` [Located 2 (Expression "first"),
                          Located 4 (Expression "second")])