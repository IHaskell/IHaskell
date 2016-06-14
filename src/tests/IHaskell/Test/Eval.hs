{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
module IHaskell.Test.Eval (testEval) where

import           Prelude

import           Data.List (stripPrefix)
import           Control.Monad (when, forM_)
import           Data.IORef (newIORef, modifyIORef, readIORef)
import           System.Directory (getTemporaryDirectory, setCurrentDirectory)

import           Data.String.Here (hereLit)

import qualified GHC.Paths

import           Test.Hspec

import           IHaskell.Test.Util (strip)
import           IHaskell.Eval.Evaluate (interpret, evaluate)
import           IHaskell.Types (EvaluationResult(..), defaultKernelState, KernelState(..),
                                 LintStatus(..), Display(..), extractPlain)

eval :: String -> IO ([Display], String)
eval string = do
  outputAccum <- newIORef []
  pagerAccum <- newIORef []
  let publish evalResult =
        case evalResult of
          IntermediateResult{} -> return ()
          FinalResult outs page [] -> do
            modifyIORef outputAccum (outs :)
            modifyIORef pagerAccum (page :)
      noWidgetHandling s _ = return s

  getTemporaryDirectory >>= setCurrentDirectory
  let state = defaultKernelState { getLintStatus = LintOff }
  interpret GHC.Paths.libdir False $ const $
    IHaskell.Eval.Evaluate.evaluate state string publish noWidgetHandling
  out <- readIORef outputAccum
  pagerOut <- readIORef pagerAccum
  return (reverse out, unlines . map extractPlain . reverse $ pagerOut)

becomes :: String -> [String] -> IO ()
becomes string expected = evaluationComparing comparison string
  where
    comparison :: ([Display], String) -> IO ()
    comparison (results, pageOut) = do
      when (length results /= length expected) $
        expectationFailure $ "Expected result to have " ++ show (length expected)
                                                           ++ " results. Got " ++ show results

      forM_ (zip results expected) $ \(ManyDisplay [Display result], expected) -> case extractPlain result of
        ""  -> expectationFailure $ "No plain-text output in " ++ show result ++ "\nExpected: " ++ expected
        str -> str `shouldBe` expected

evaluationComparing :: (([Display], String) -> IO b) -> String -> IO b
evaluationComparing comparison string = do
  let indent (' ':x) = 1 + indent x
      indent _ = 0
      empty = null . strip
      stringLines = filter (not . empty) $ lines string
      minIndent = minimum (map indent stringLines)
      newString = unlines $ map (drop minIndent) stringLines
  eval newString >>= comparison

pages :: String -> [String] -> IO ()
pages string expected = evaluationComparing comparison string
  where
    comparison (results, pageOut) =
      strip (stripHtml pageOut) `shouldBe` strip (fixQuotes $ unlines expected)

    -- A very, very hacky method for removing HTML
    stripHtml str = go str
      where
        go ('<':str) =
          case stripPrefix "script" str of
            Nothing  -> go' str
            Just str -> dropScriptTag str
        go (x:xs) = x : go xs
        go [] = []

        go' ('>':str) = go str
        go' (x:xs) = go' xs
        go' [] = error $ "Unending bracket html tag in string " ++ str

        dropScriptTag str =
          case stripPrefix "</script>" str of
            Just str -> go str
            Nothing  -> dropScriptTag $ tail str

    fixQuotes :: String -> String
#if MIN_VERSION_ghc(7, 8, 0)
    fixQuotes = id
#else
    fixQuotes = map $ \char -> case char of
      '\8216' -> '`'
      '\8217' -> '\''
      c       -> c
#endif


testEval :: Spec
testEval =
  describe "Code Evaluation" $ do
    it "evaluates expressions" $ do
      "3" `becomes` ["3"]
      "3+5" `becomes` ["8"]
      "print 3" `becomes` ["3"]
      [hereLit|
        let x = 11
            z = 10 in
          x+z
      |] `becomes` ["21"]

    it "evaluates flags" $ do
      ":set -package hello" `becomes` ["Warning: -package not supported yet"]
      ":set -XNoImplicitPrelude" `becomes` []

    it "evaluates multiline expressions" $ do
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

    it "prints Unicode characters correctly" $ do
      "putStrLn \"Héllö, Üñiço∂e!\"" `becomes` ["Héllö, Üñiço∂e!"]
      "putStrLn \"Привет!\"" `becomes` ["Привет!"]

    it "evaluates directives" $ do
      ":typ 3" `becomes` ["3 :: forall a. Num a => a"]
      ":k Maybe" `becomes` ["Maybe :: * -> *"]
      ":in String" `pages` ["type String = [Char] \t-- Defined in \8216GHC.Base\8217"]
