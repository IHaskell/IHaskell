{-|
Module      : Jupyter.IHaskell.Test.Parser
Description : Tests for Jupyter.IHaskell.Parser.
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
module Jupyter.IHaskell.Test.Parser (parserTests) where

-- Imports from 'base'
import           Control.Exception (bracket)
import           Control.Monad (when, unless, void)
import           System.Environment (getEnv, setEnv)


-- Imports from 'text'
import           Data.Text (Text)
import qualified Data.Text as T

-- Imports from 'directory'
import           System.Directory (createDirectoryIfMissing)

-- Imports from 'extra'
import           System.Directory.Extra (withCurrentDirectory)
import           System.IO.Extra (withTempDir)

-- Imports from 'tasty'
import           Test.Tasty (TestTree, testGroup)

-- Imports from 'tasty-hunit'
import           Test.Tasty.HUnit (testCase, assertFailure, assertEqual)

-- Imports from 'transformers'
import           Control.Monad.IO.Class (MonadIO(..))

-- Imports from 'ghc'
import           GHC.Paths (libdir)

-- Imports from 'ihaskell'
import           Jupyter.IHaskell.Parser (Loc(..), CodeBlock(..), parseCodeBlocks, ParseError(..),
                                          DirectiveType(..))
import           Jupyter.IHaskell.Interpreter (runInterpreter, Interpreter)
import           Jupyter.IHaskell.Evaluate (setExtension)

parserTests :: TestTree
parserTests =
  testGroup "Parser"
    [expressionTests, quasiquoteTests, declarationTests, directiveTests, importTests, pragmaTests]

test :: String -> Interpreter a -> TestTree
test name action = testCase name $ void $ runInterpreter (Just libdir) action

expressionTests :: TestTree
expressionTests = test "Expressions" $ do
  "first\n\nsecond\nthird\n\nfourth" -->
    [ Loc 1 (Expression "first")
    , Loc 3 (Expression "second")
    , Loc 4 (Expression "third")
    , Loc 6 (Expression "fourth")
    ]
  "" --> []
  "3 + 5" --> [expr 1 "3 + 5"]
  "3\nlet x = expr" --> [expr 1 "3", stmt 2 "let x = expr"]
  "let x = 3 in x + 3" --> [expr 1 "let x = 3 in x + 3"]
  "y <- print 'no'" --> [stmt 1 "y <- print 'no'"]
  "y <- do print 'no'\nlet x = expr" -->
    [stmt 1 "y <- do print 'no'", stmt 2 "let x = expr"]
  "y <- do print 'no'\nlet x = expr\nexpression" --> 
    [stmt 1 "y <- do print 'no'", stmt 2 "let x = expr", expr 3 "expression"]
  "print yes\nprint no" --> [expr 1 "print yes", expr 2 "print no"]
  "let x = 3 in" `errors`
    ParseError "parse error (possibly incorrect indentation or mismatched brackets)\n" 1 13

  let longString = concat $ replicate 20 "hello "
      testExpr = T.pack $ "img ! src \"" ++ longString ++ "\" ! width \"500\""
  testExpr --> [expr 1 testExpr]
  "show (show (do\n  Just 10\n  Nothing\n  Just 100))" -->
    [expr 1 "show (show (do\n  Just 10\n  Nothing\n  Just 100))"]

pragmaTests :: TestTree
pragmaTests = test "Pragmas" $ do
  "{-# LANGUAGE QuasiQuotes #-}" --> [lang 1 ["QuasiQuotes"]]
  "{-# LANGUAGE QuasiQuotes,X #-}" --> [lang 1 ["QuasiQuotes", "X"]]
  "{-# LANguage QuasiQuotes,X #-}" --> [lang 1 ["QuasiQuotes", "X"]]
  "{-# language QuasiQuotes,X #-}" --> [lang 1 ["QuasiQuotes", "X"]]
  "{-#   language     QuasiQuotes,X #-}" --> [lang 1 ["QuasiQuotes", "X"]]
  "\n{-#   language     QuasiQuotes,X #-}" --> [lang 2 ["QuasiQuotes", "X"]]
  "{-# PRAGMA other #-}" `errors` ParseError "Unsupported pragma: PRAGMA" 1 1

quasiquoteTests :: TestTree
quasiquoteTests = test "Quasiquotes" $ do
  setExtension "QuasiQuotes"
  "let x = [q|a quasiquote|]" --> [stmt 1 "let x = [q|a quasiquote|]"]
  "let x = [q|a quasiquote|]\n3" --> [stmt 1 "let x = [q|a quasiquote|]", expr 2 "3"]
  "let x = [q|a quasiquote\n|]" --> [stmt 1 "let x = [q|a quasiquote\n|]"]
  "let x = [q|\na quasiquote\n|]" --> [stmt 1 "let x = [q|\na quasiquote\n|]"]
  "let x = \"[q|doesn't matter\"" --> [stmt 1 "let x = \"[q|doesn't matter\""]
  "[q|q<-[1..10]]" `errors` ParseError "unterminated quasiquotation at end of input\n" 1 4
  "[q|x|] [q|x|]" --> [expr 1 "[q|x|] [q|x|]"]
  "[q|\nx\n|] [q|x|]" --> [expr 1 "[q|\nx\n|] [q|x|]"]

importTests :: TestTree
importTests = test "Imports" $ do
  "import Data.Monoid" --> [imp 1 "import Data.Monoid"]
  "import Control.Applicative" --> [imp 1 "import Control.Applicative"]
  "import Parser" --> [imp 1 "import Parser"]
  "\n\nimport Parser" --> [imp 3 "import Parser"]
  "\nimport GHC\nimport Parser" --> [imp 2 "import GHC", imp 3 "import Parser"]
  "\nimport GHC\n\nimport Parser" --> [imp 2 "import GHC", imp 4 "import Parser"]
  "\nimport qualified GHC" --> [imp 2 "import qualified GHC"]
  "\nimport qualified GHC.X.Y" --> [imp 2 "import qualified GHC.X.Y"]
  "\nimport qualified GHC.X.Y as Z" --> [imp 2 "import qualified GHC.X.Y as Z"]
  "import Control.Applicative()" --> [imp 1 "import Control.Applicative()"]
  "import Control.Applicative ()" --> [imp 1 "import Control.Applicative ()"]
  "import Control.Applicative (\n  )" --> [imp 1 "import Control.Applicative (\n  )"]
  "import Control.Applicative (x, y\n  )" --> [imp 1 "import Control.Applicative (x, y\n  )"]
  "import Control.Applicative (x, y,\n  z)" --> [imp 1 "import Control.Applicative (x, y,\n  z)"]
  "import Data.Monoid hiding ()" --> [imp 1 "import Data.Monoid hiding ()"]
  "import Data.Monoid hiding (a, b)" --> [imp 1 "import Data.Monoid hiding (a, b)"]
  "import Data.Monoid hiding (a,\n b)" --> [imp 1 "import Data.Monoid hiding (a,\n b)"]

  "import Data.Monoid hiding (" `errors`
    ParseError "parse error (possibly incorrect indentation or mismatched brackets)\n" 1 28

  "import X\nprint 3" --> [imp 1 "import X", expr 2 "print 3"]
  "import X\n\nprint 3" --> [imp 1 "import X", expr 3 "print 3"]


directiveTests :: TestTree
directiveTests = test "Directives" $ do
  ":type x\n:ty x" --> [directive 1 GetType "x", directive 2 GetType "x"]
  ":typ x\n:t x" --> [directive 1 GetType "x", directive 2 GetType "x"]
  ":info x\n:in x" --> [directive 1 GetInfo "x", directive 2 GetInfo "x"]
  ":inf x\n:i x" --> [directive 1 GetInfo "x", directive 2 GetInfo "x"]
  ":? x\n:help x" --> [directive 1 GetHelp "x", directive 2 GetHelp "x"]
  ":hel x\n:he x" --> [directive 1 GetHelp "x", directive 2 GetHelp "x"]
  ":h x" --> [directive 1 GetHelp "x"]
  ":set x" --> [directive 1 SetDynFlag "x"]

  ":hoogle x\n:hoog x\n:ho x" -->
    [directive 1 SearchHoogle "x", directive 2 SearchHoogle "x", directive 3 SearchHoogle "x"]
  ":document x\n:doc x\n:d x" -->
    [directive 1 GetDoc "x", directive 2 GetDoc "x", directive 3 GetDoc "x"]
  ":kind x\n:kin x\n:k x" -->
    [directive 1 GetKind "x", directive 2 GetKind "x", directive 3 GetKind "x"]
  ":module x\n:mod x\n:m x" -->
    [directive 1 LoadModule "x", directive 2 LoadModule "x", directive 3 LoadModule "x"]

  ":ex x\n:extension x" `errors` ParseError "Unknown directive: ex" 1 1
  ":opt x\n:option x" `errors` ParseError "Unknown directive: opt" 1 1
  ":nope goodbye" `errors` ParseError "Unknown directive: nope" 1 1

  "3\n:t expr" --> [expr 1 "3", directive 2 GetType "expr"]

declarationTests :: TestTree
declarationTests = test "Declarations" $ do
  "data X = Y Int" --> [decl 1 "data X = Y Int"]
  "data X = Y Int\ndata X = Y Int" --> [decl 1 "data X = Y Int\ndata X = Y Int"]
  "data X" --> [decl 1 "data X"]
  "data X\ndata Y" --> [decl 1 "data X\ndata Y"]

  "fun [] = 10" --> [decl 1 "fun [] = 10"]
  "fun [] = 10\nprint 'h'" --> [decl 1 "fun [] = 10", expr 2 "print 'h'"]
  "fun (x : xs) = 100" --> [decl 1 "fun (x : xs) = 100"]
  "fun [] = 10\nfun (x : xs) = 100" --> [decl 1 "fun [] = 10\nfun (x : xs) = 100"]
  "fun :: [a] -> Int\nfun [] = 10\nfun (x : xs) = 100" -->
    [decl 1 "fun :: [a] -> Int\nfun [] = 10\nfun (x : xs) = 100"]
  "data X = 3" `errors`
    ParseError "Cannot parse data constructor in a data/newtype declaration: 3\n" 1 10
  "test arg = hello\n  where\n    x = y\n\n    z = w" -->
    [decl 1 "test arg = hello\n  where\n    x = y\n\n    z = w"]

  "data X = Int :+ X" --> [decl 1 "data X = Int :+ X"]
  "data X = Int + X" `errors` ParseError "parse error on input \8216+\8217\n" 1 14

  "(+++) :: Int -> Int -> Int" --> [decl 1 "(+++) :: Int -> Int -> Int"]
  "(+++) :: Int -> Int -> Int\n(+++) a b = a" --> [decl 1 "(+++) :: Int -> Int -> Int\n(+++) a b = a"]
  "(+++) :: Int -> Int -> Int\na +++ b = a" --> [decl 1 "(+++) :: Int -> Int -> Int\na +++ b = a"]

imp :: Int -> Text -> Loc CodeBlock
imp line text = Loc line $ Import text

stmt :: Int -> Text -> Loc CodeBlock
stmt line text = Loc line $ Statement text

decl :: Int -> Text -> Loc CodeBlock
decl line text = Loc line $ Declarations text

directive :: Int -> DirectiveType -> Text -> Loc CodeBlock
directive line dtype text = Loc line $ Directive dtype text

expr :: Int -> Text -> Loc CodeBlock
expr line text = Loc line $ Expression text

lang :: Int -> [Text] -> Loc CodeBlock
lang line exts = Loc line $ LanguagePragma exts

(-->) :: Text -> [Loc CodeBlock] -> Interpreter ()
text --> expected = do
  result <- parseCodeBlocks text
  case result of
    Left err -> liftIO $ assertFailure $
      concat ["Parse Error for input:\n", T.unpack text, "\n", "Error: ", show err]
    Right observed ->
      unless (and (zipWith (==) expected observed) && length expected == length observed) $
        liftIO $ assertFailure $
          concat
            [ "Did not get expected parse for: "
            , show text
            , "\nExpected: "
            , show expected
            , "\nObserved: "
            , show observed
            ]

errors :: Text -> ParseError -> Interpreter ()
errors text errExpected = do
  result <- parseCodeBlocks text
  case result of
    Left errObserved ->
      liftIO $ assertEqual "Did not get expected parse error" errExpected errObserved
    Right observed ->
      liftIO $ assertFailure $
        concat ["Expected error for input: ", T.unpack text, "\nInstead got: ", show observed]
