{-# LANGUAGE QuasiQuotes #-}
import GHC
import GHC.Paths
import Data.IORef
import Control.Monad
import Data.List
import System.Directory
import Data.String.Here
import Data.String.Utils (strip)

import IHaskell.Eval.Parser
import IHaskell.Types
import IHaskell.IPython
import IHaskell.Eval.Evaluate

import Test.Hspec
import Test.Hspec.HUnit

doGhc = runGhc (Just libdir)

parses = doGhc . parseString

like parser desired = parser >>= (`shouldBe` desired)

is string blockType = do
  result <- doGhc $ parseString string
  result `shouldBe` [blockType string]

eval string = do
  outputAccum <- newIORef []
  let publish displayDatas = liftIO $ modifyIORef outputAccum (displayDatas :)
  getTemporaryDirectory >>= setCurrentDirectory
  interpret $ evaluate 1 string publish
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



main :: IO ()
main = hspec $ do
  parserTests
  ipythonTests
  evalTests

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

ipythonTests = do
  describe "Parse IPython Version" $ do
    it "parses 2.0.0-dev" $
      parseVersion "2.0.0-dev" `shouldBe` [2, 0, 0]
    it "parses 2.0.0-alpha" $
      parseVersion "2.0.0-dev" `shouldBe` [2, 0, 0]
    it "parses 12.5.10" $
      parseVersion "12.5.10" `shouldBe` [12, 5, 10]

parserTests = do
  splitAtLocTests
  layoutChunkerTests
  moduleNameTests
  parseStringTests


splitAtLocTests = describe "String Splitting Util" $ do
  it "splits properly (example 1)" $
    splitAtLoc 2 3 "abc\ndefghi\nxyz\n123" `shouldBe` ("abc\nde","fghi\nxyz\n123")
  it "splits properly (example 2)" $
    splitAtLoc 2 1 "abc" `shouldBe` ("abc","")
  it "splits properly (example 3)" $
    splitAtLoc 2 1 "abc\nhello" `shouldBe` ("abc\n","hello")

layoutChunkerTests = describe "Layout Chunk" $ do
  it "chunks 'a string'" $
    layoutChunks "a string" `shouldBe` ["a string"]

  it "chunks 'a\\nstring'" $
    layoutChunks "a\n string" `shouldBe` ["a\n string"]

  it "chunks 'a\\n string\\nextra'" $
    layoutChunks "a\n string\nextra" `shouldBe` ["a\n string","extra"]

  it "chunks strings with too many lines" $
    layoutChunks "a\n\nstring" `shouldBe` ["a","string"]

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
      Directive HelpForSet "x"
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
    
