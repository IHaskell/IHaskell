{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP         #-}
module IHaskell.Test.Parser (testParser) where

import           Prelude

import           Text.RawString.QQ (r)

import           Test.Hspec
import           Test.HUnit (assertFailure)

import           IHaskell.Test.Util (ghc, strip)
import           IHaskell.Eval.Parser (parseString, getModuleName, unloc, layoutChunks, Located(..),
                                       CodeBlock(..), DirectiveType(..), StringLoc(..), PragmaType(..))
import           IHaskell.Eval.ParseShell (parseShell)

parses :: String -> IO [CodeBlock]
parses str = map unloc <$> ghc (parseString str)

like :: (Show a, Eq a) => IO a -> a -> IO ()
like parser desired = parser >>= (`shouldBe` desired)

is :: String -> (String -> CodeBlock) -> IO ()
is string blockType = do
  result <- ghc $ parseString string
  map unloc result `shouldBe` [blockType $ strip string]

testParser :: Spec
testParser = do
  testLayoutChunks
  testModuleNames
  testParseString
  testParseShell

testLayoutChunks :: Spec
testLayoutChunks = describe "Layout Chunk" $ do
  it "chunks 'a string'" $
    map unloc (layoutChunks "a string") `shouldBe` ["a string"]

  it "chunks 'a\\n string'" $
    map unloc (layoutChunks "a\n string") `shouldBe` ["a\n string"]

  it "chunks 'a\\n string\\nextra'" $
    map unloc (layoutChunks "a\n string\nextra") `shouldBe` ["a\n string", "extra"]

  it "chunks strings with too many lines" $
    map unloc (layoutChunks "a\n\nstring") `shouldBe` ["a", "string"]

  it "parses multiple exprs" $ do
    let text = [r|
                 first

                 second
                 third

                 fourth
               |]
    layoutChunks text `shouldBe` [ Located 2 "first"
                                 , Located 4 "second"
                                 , Located 5 "third"
                                 , Located 7 "fourth"
                                 ]
  it "deals with quasiquotes" $ do
    let parsesAsBlocks strs = map unloc (layoutChunks $ unlines strs) `shouldBe` strs
    parsesAsBlocks ["let x = [q|a quasiquote|]"]
    parsesAsBlocks ["let x = [q|a quasiquote|]", "3"]
    parsesAsBlocks ["let x = [q|a quasiquote\n|]"]
    parsesAsBlocks ["let x = [q|\na quasiquote\n|]"]
    parsesAsBlocks ["let x = \"[q|doesn't matter\""]
    parsesAsBlocks ["[q|q<-[1..10]]"]
    parsesAsBlocks ["[q|x|] [q|x|]"]
    parsesAsBlocks ["[q|\nx\n|] [q|x|]"]


testModuleNames :: Spec
testModuleNames = describe "Get Module Name" $ do
  it "parses simple module names" $
    "module A where\nx = 3" `named` ["A"]
  it "parses module names with dots" $
    "module A.B where\nx = 3" `named` ["A", "B"]
  it "parses module names with exports" $
    "module A.B.C ( x ) where x = 3" `named` ["A", "B", "C"]
  it "errors when given unnamed modules" $ do
    ghc (getModuleName "x = 3") `shouldThrow` anyException
  where
    named str result = do
      res <- ghc $ getModuleName str
      res `shouldBe` result


testParseShell :: Spec
testParseShell =
  describe "Parsing Shell Commands" $ do
    test "A" ["A"]
    test ":load A" [":load", "A"]
    test ":!l ~/Downloads/MyFile\\ Has\\ Spaces.txt"
      [":!l", "~/Downloads/MyFile\\ Has\\ Spaces.txt"]
    test ":!l \"~/Downloads/MyFile Has Spaces.txt\" /Another/File\\ WithSpaces.doc"
      [":!l", "~/Downloads/MyFile Has Spaces.txt", "/Another/File\\ WithSpaces.doc"]
  where
    test string expected =
      it ("parses " ++ string ++ " correctly") $
        string `shouldParseTo` expected

    shouldParseTo xs ys =
      case parseShell xs of
        Right xs' -> xs' `shouldBe` ys
        Left e    -> assertFailure $ "parseShell returned error: \n" ++ show e

testParseString :: Spec
testParseString = describe "Parser" $ do
  it "parses empty strings" $
    parses "" `like` []

  it "parses simple imports" $
    "import Data.Monoid" `is` Import

  it "parses simple arithmetic" $
    "3 + 5" `is` Expression

  it "parses :type" $
    parses ":type x\n:ty x" `like` [Directive GetType "x", Directive GetType "x"]

  it "parses :info" $
    parses ":info x\n:in x" `like` [Directive GetInfo "x", Directive GetInfo "x"]

  it "parses :help and :?" $
    parses ":? x\n:help x" `like` [Directive GetHelp "x", Directive GetHelp "x"]

  it "parses :set x" $
    parses ":set x" `like` [Directive SetDynFlag "x"]

  it "parses :extension x" $
    parses ":ex x\n:extension x" `like` [Directive SetExtension "x", Directive SetExtension "x"]

  it "fails to parse :nope" $
    parses ":nope goodbye" `like` [ParseError (Loc 1 1) "Unknown directive: 'nope'."]

  it "parses number followed by let stmt" $
    parses "3\nlet x = expr" `like` [Expression "3", Statement "let x = expr"]

  it "parses let x in y" $
    "let x = 3 in x + 3" `is` Expression

  it "parses a data declaration" $
    "data X = Y Int" `is` Declaration

  it "parses number followed by type directive" $
    parses "3\n:t expr" `like` [Expression "3", Directive GetType "expr"]

  it "parses a <- statement" $
    "y <- print 'no'" `is` Statement

  it "parses a <- stmt followed by let stmt" $
    parses "y <- do print 'no'\nlet x = expr" `like` [ Statement "y <- do print 'no'"
                                                     , Statement "let x = expr"
                                                     ]

  it "parses <- followed by let followed by expr" $
    parses "y <- do print 'no'\nlet x = expr\nexpression" `like` [ Statement "y <- do print 'no'"
                                                                 , Statement "let x = expr"
                                                                 , Expression "expression"
                                                                 ]

  it "parses two print statements" $
    parses "print yes\nprint no" `like` [Expression "print yes", Expression "print no"]

  it "parses a pattern-maching function declaration" $
    "fun [] = 10" `is` Declaration

  it "parses a function decl followed by an expression" $
    parses "fun [] = 10\nprint 'h'" `like` [Declaration "fun [] = 10", Expression "print 'h'"]

  it "parses list pattern matching fun decl" $
    "fun (x : xs) = 100" `is` Declaration

  it "parses two pattern matches as the same declaration" $
    "fun [] = 10\nfun (x : xs) = 100" `is` Declaration

  it "parses a type signature followed by a declaration" $
    "fun :: [a] -> Int\nfun [] = 10\nfun (x : xs) = 100" `is` Declaration

  it "parses a simple module" $
    "module A where x = 3" `is` Module

  it "parses a module with an export" $
    "module B (x) where x = 3" `is` Module

  it "breaks when a let is incomplete" $
    parses "let x = 3 in" `like` [ ParseError (Loc 1 13)
                                     "parse error (possibly incorrect indentation or mismatched brackets)"
                                 ]

  it "parses LANGUAGE pragmas case-insensitively" $
    parses "{-# LaNgUaGe OverloadedStrings #-}" `like` [Pragma PragmaLanguage ["OverloadedStrings"]]

  it "breaks without data kinds" $
    parses "data X = 3" `like` [dataKindsError]

  it "parses statements after imports" $ do
    parses "import X\nprint 3" `like` [Import "import X", Expression "print 3"]
    parses "import X\n\nprint 3" `like` [Import "import X", Expression "print 3"]
  it "ignores blank lines properly" $
    [r|
      test arg = hello
        where
          x = y

          z = w
    |] `is` Declaration
  it "doesn't break on long strings" $ do
    let longString = concat $ replicate 20 "hello "
    ("img ! src \"" ++ longString ++ "\" ! width \"500\"") `is` Expression

  it "parses do blocks in expression" $ do
    [r|
      show (show (do
        Just 10
        Nothing
        Just 100))
    |] `is` Expression
  it "correctly locates parsed items" $ do
    ghc (parseString
      [r|
        first

        second
       |]) >>= (`shouldBe` [Located 2 (Expression "first"), Located 4 (Expression "second")])
  where
#if MIN_VERSION_ghc(8,10,0)
    dataKindsError = ParseError (Loc 1 11) msg
#else
    dataKindsError = ParseError (Loc 1 10) msg
#endif
#if MIN_VERSION_ghc(9,2,0)
    msg = "Cannot parse data constructor in a data/newtype declaration: 3"
#elif MIN_VERSION_ghc(8,8,0)
    msg = "Cannot parse data constructor in a data/newtype declaration:\n  3"
#else
    msg = "Cannot parse data constructor in a data/newtype declaration: 3"
#endif
