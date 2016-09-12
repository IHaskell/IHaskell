{-|
Module      : Jupyter.IHaskell.Test.Hoogle
Description : Tests for Jupyter.IHaskell.Hoogle
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings #-}
module Jupyter.IHaskell.Test.Hoogle (hoogleTests) where

-- Imports from 'base'
import           Control.Monad (unless)

-- Imports from 'text'
import           Data.Text (Text)
import qualified Data.Text as T

-- Imports from 'tasty'
import           Test.Tasty (TestTree)

-- Imports from 'tasty-hunit'
import           Test.Tasty.HUnit (testCase, assertFailure)

-- Imports from 'transformers'
import           Control.Monad.IO.Class (MonadIO(..))

-- Imports from 'ihaskell'
import           Jupyter.IHaskell.Hoogle (hoogleSearch, hoogleSearchExact, HoogleResult(..))

-- | Type of Hoogle query to issue.
data Query = ExactMatch Text -- ^ Search for a string
           | InexactMatch Text -- ^ Search for a precise identifier match
  deriving (Eq, Ord, Show)

-- | Test the IHaskell HLint integration.
hoogleTests :: TestTree
hoogleTests = testCase "Hoogle" $ do
  -- Exact matches.
  ExactMatch "map" --> [ hoogle "base" "Prelude.html#v:map" "map :: (a -> b) -> [a] -> [b]"
                           "map f xs is the list obtained by applying f to each element"
                       ]
  ExactMatch "fmap" --> [HoogleResult "" "" ""]
  ExactMatch "Data.Text" --> [HoogleResult "" "" ""]
  ExactMatch "base" --> [HoogleResult "" "" ""]
  ExactMatch "FromJSON" --> [HoogleResult "" "" ""]
  ExactMatch "Monad" --> [HoogleResult "" "" ""]

  -- Inexact matches.
  InexactMatch "map" --> [HoogleResult "" "" ""]
  InexactMatch "fmap" --> [HoogleResult "" "" ""]
  InexactMatch "Data.Text" --> [HoogleResult "" "" ""]
  InexactMatch "base" --> [HoogleResult "" "" ""]
  InexactMatch "Monad" --> [HoogleResult "" "" ""]

  -- Type signature searches.
  InexactMatch ":: a -> a" --> [HoogleResult "" "" ""]
  InexactMatch ":: Monoid w => w -> w -> w" --> [HoogleResult "" "" ""]

hoogle :: Text -> Text -> Text -> Text -> HoogleResult
hoogle pkg url =
  HoogleResult
    (T.concat ["http://hackage.haskell.org/packages/archive/", pkg, "/latest/doc/html/", url])


-- | Run a single linter test. Lint the input and then compare output suggestions to generated
-- suggestions.
(-->) :: Query -> [HoogleResult] -> IO ()
query --> expected = do
  result <- case query of
              ExactMatch txt   -> hoogleSearch txt
              InexactMatch txt -> hoogleSearchExact txt
  case result of
    Left err -> assertFailure $ "Hoogle Error: " ++ err
    Right observed ->
      unless (and (zipWith sameResult expected observed) && length expected <= length observed) $
        assertFailure $
          concat
            [ "Did not get expected Hoogle results for: "
            , show query
            , "\nExpected: "
            , show expected
            , "\nObserved: "
            , show observed
            ]

  where
    sameResult (HoogleResult url1 self1 doc1) (HoogleResult url2 self2 doc2) =
      url1 == url2 &&
      self1 == self2 &&
      and (zipWith (==) (T.unpack doc1) (T.unpack doc2)) &&
      T.length doc1 <= T.length doc2
