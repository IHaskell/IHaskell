{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IHaskell.Test.Eval (testEval) where

import           Prelude

import           Control.Monad (when, forM_)
import           Data.Aeson (encode)
import           Data.IORef (newIORef, modifyIORef, readIORef)
import qualified Data.Text as Text
import           System.Directory (getTemporaryDirectory, setCurrentDirectory, getCurrentDirectory)

import           Text.RawString.QQ (r)

import qualified GHC.Paths

import           Test.Hspec

import           IHaskell.Eval.Evaluate (interpret, evaluate, liftIO)
import           IHaskell.Test.Util (strip)
import           IHaskell.Types (Display(..), DisplayData(..), EvaluationResult(..), KernelState(..),
                                 LintStatus(..), MimeType(..), defaultKernelState, extractPlain)

eval :: String -> IO ([Display], String)
eval string = do
  outputAccum <- newIORef []
  pagerAccum <- newIORef []
  let publish evalResult _ =
        case evalResult of
          IntermediateResult{} -> return ()
          FinalResult outs page _ -> do
            modifyIORef outputAccum (outs :)
            modifyIORef pagerAccum (page :)
      noWidgetHandling s _ = return s
  let state = defaultKernelState { getLintStatus = LintOff }

  liftIO $ getTemporaryDirectory >>= setCurrentDirectory
  _ <- interpret GHC.Paths.libdir False False $ \hasSupportLibraries -> do
        IHaskell.Eval.Evaluate.evaluate
          (state { supportLibrariesAvailable = hasSupportLibraries })
          string publish noWidgetHandling

  out <- readIORef outputAccum
  pagerout <- readIORef pagerAccum
  return (reverse out, unlines . map extractPlain . reverse $ pagerout)

displayDatasBecome :: String -> [Display] -> IO ()
displayDatasBecome command desired = do
  (displays, _output) <- eval command
  when (displays /= desired) $
    expectationFailure $ "Expected display datas to be " ++ show (encode desired)
                         ++ ". Got " ++ show (encode displays)

becomes :: String -> [String] -> IO ()
becomes string expected = evaluationComparing comparison string
  where
    comparison :: ([Display], String) -> IO ()
    comparison (results, _pageOut) = do
      when (length results /= length expected) $
        expectationFailure $ "Expected result to have " ++ show (length expected)
                                                        ++ " results. Got " ++ show (encode results)

      forM_ (zip results expected) $ \(result, expect) ->
        case getPlain result of
          "" -> expectationFailure $ "No plain-text output in " ++ show result ++ "\nExpected: " ++ expect
          str -> str `shouldBe` expect

    getPlain :: Display -> String
    getPlain (Display datas)
      = filter (/= '\r') -- normalise Windows line endings
      $ extractPlain datas
    getPlain (ManyDisplay disps) = concatMap getPlain disps

evaluationComparing :: (([Display], String) -> IO b) -> String -> IO b
evaluationComparing comparison string = do
  let indent (' ':x) = 1 + indent x
      indent _ = 0
      empty = null . strip
      stringLines = filter (not . empty) $ lines string
      minIndent = minimum (map indent stringLines)
      newString = unlines $ map (drop minIndent) stringLines
  eval newString >>= comparison


testEval :: Spec
testEval =
  describe "Code Evaluation" $ do
    it "gets rid of the test failure with Nix" $
      let
        throwAway :: String -> [String] -> IO ()
        throwAway string _ =
          evaluationComparing (const $ shouldBe True True) string
      in throwAway "True" ["True"]

    it "evaluates expressions" $ do
      "3" `becomes` ["3"]
      "3+5" `becomes` ["8"]
      "print 3" `becomes` ["3"]
      [r|
        let x = 11
            z = 10 in
          x+z
      |] `becomes` ["21"]

    it "evaluates :set -package" $ do
      ":set -package hello" `becomes` ["Warning: -package not supported yet"]

    -- it "evaluates :set -XNoImplicitPrelude" $ do
    --   ":set -XNoImplicitPrelude" `becomes` []

    it "evaluates multiline expressions" $ do
      [r|
        import Control.Monad
        forM_ [1, 2, 3] $ \x ->
          print x
      |] `becomes` ["1\n2\n3"]

    it "evaluates function declarations silently" $ do
      [r|
        fun :: [Int] -> Int
        fun [] = 3
        fun (x:xs) = 10
        fun [1, 2]
      |] `becomes` ["10"]

    it "evaluates data declarations" $ do
      [r|
        data X = Y Int
               | Z String
               deriving (Show, Eq)
        print [Y 3, Z "No"]
        print (Y 3 == Z "No")
      |] `becomes` ["[Y 3,Z \"No\"]", "False"]

    it "evaluates do blocks in expressions" $ do
      [r|
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

    it "prints multiline output correctly" $ do
      ":! printf \"hello\\nworld\"" `becomes` ["hello\nworld"]

    it "evaluates :typ directive" $ do
#if MIN_VERSION_ghc(9,2,0)
      -- It's `a` instead of `p`
      ":typ 3" `becomes` ["3 :: forall {a}. Num a => a"]
#elif MIN_VERSION_ghc(9,0,0)
      -- brackets around the type variable
      ":typ 3" `becomes` ["3 :: forall {p}. Num p => p"]
#else
      -- It's `p` instead of `t` for some reason
      ":typ 3" `becomes` ["3 :: forall p. Num p => p"]
#endif

    it "evaluates :k directive" $ do
      ":k Maybe" `becomes` ["Maybe :: * -> *"]

    it "evaluates :in directive" $ do
      (displays, _) <- eval ":in String"
      case displays of
        [ManyDisplay [Display [DisplayData PlainText plain, DisplayData MimeHtml html]]] -> do
          -- The type definition is stable; the module name and whether quotation
          -- marks use unicode vary across GHC versions and platforms, so don't
          -- check everything.
          Text.unpack plain `shouldContain` "type String = [Char]"
          Text.unpack html  `shouldContain` "<span class=\"cm-keyword\">type</span>"
          Text.unpack html  `shouldContain` "<span class=\"cm-variable-2\">String</span>"
          Text.unpack html  `shouldContain` "<span class=\"cm-variable-2\">Char</span>"
        _ -> expectationFailure $ "Unexpected display structure for :in String: "
                                    ++ show (encode displays)

    it "captures stderr" $ do
      [r|
        import Debug.Trace
        trace "test" 5
      |] `becomes` ["test\n5"]

    it "immediately applies language extensions" $ do
      [r|
        {-# LANGUAGE RankNTypes #-}

        identity :: forall a. a -> a
        identity a = a
      |] `becomes` []
