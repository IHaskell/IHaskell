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
  let publish evalResult _ =
        case evalResult of
          IntermediateResult{} -> return ()
          FinalResult outs page _ -> do
            modifyIORef outputAccum (outs :)
            modifyIORef pagerAccum (page :)
      noWidgetHandling s _ = return s

  getTemporaryDirectory >>= setCurrentDirectory
  let state = defaultKernelState { getLintStatus = LintOff }
  _ <- interpret GHC.Paths.libdir False False $ const $
        IHaskell.Eval.Evaluate.evaluate state string publish noWidgetHandling
  out <- readIORef outputAccum
  pagerout <- readIORef pagerAccum
  return (reverse out, unlines . map extractPlain . reverse $ pagerout)

becomes :: String -> [String] -> IO ()
becomes string expected = evaluationComparing comparison string
  where
    comparison :: ([Display], String) -> IO ()
    comparison (results, _pageOut) = do
      when (length results /= length expected) $
        expectationFailure $ "Expected result to have " ++ show (length expected)
                                                           ++ " results. Got " ++ show results

      forM_ (zip results expected) $ \(ManyDisplay [Display result], expect) -> case extractPlain result of
        ""  -> expectationFailure $ "No plain-text output in " ++ show result ++ "\nExpected: " ++ expect
        str -> str `shouldBe` expect

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
    comparison (_results, pageOut) =
      strip (stripHtml pageOut) `shouldBe` strip (fixQuotes $ unlines expected)

    -- A very, very hacky method for removing HTML
    stripHtml str = go str
      where
        go ('<':xs) =
          case stripPrefix "script" xs of
            Nothing  -> go' str
            Just s -> dropScriptTag s
        go (x:xs) = x : go xs
        go [] = []

        go' ('>':xs) = go xs
        go' (_:xs) = go' xs
        go' [] = error $ "Unending bracket html tag in string " ++ str

        dropScriptTag str1 =
          case stripPrefix "</script>" str1 of
            Just s  -> go s
            Nothing -> dropScriptTag $ tail str

    fixQuotes :: String -> String
    fixQuotes = id


testEval :: Spec
testEval =
  describe "Code Evaluation" $ do
    it "gets rid of the test failure with Nix" $
      let
        throwAway string _ =
          evaluationComparing (const $ shouldBe True True) string
      in throwAway "True" ["True"]

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

    it "prints multiline output correctly" $ do
      ":! printf \"hello\\nworld\"" `becomes` ["hello\nworld"]

    it "evaluates directives" $ do
#if MIN_VERSION_ghc(8,2,0)
      -- It's `p` instead of `t` for some reason
      ":typ 3" `becomes` ["3 :: forall p. Num p => p"]
#else
      ":typ 3" `becomes` ["3 :: forall t. Num t => t"]
#endif
      ":k Maybe" `becomes` ["Maybe :: * -> *"]
#if MIN_VERSION_ghc(8,10,0)
      ":in String" `pages` ["type String :: *\ntype String = [Char]\n  \t-- Defined in \8216GHC.Base\8217"]
#else
      ":in String" `pages` ["type String = [Char] \t-- Defined in \8216GHC.Base\8217"]
#endif

    it "captures stderr" $ do
      [hereLit|
        import Debug.Trace
        trace "test" 5
      |] `becomes` ["test\n5"]

    it "immediately applies language extensions" $ do
      [hereLit|
        {-# LANGUAGE RankNTypes #-}

        identity :: forall a. a -> a
        identity a = a
      |] `becomes` []
