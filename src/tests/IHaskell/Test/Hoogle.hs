{-# LANGUAGE QuasiQuotes #-}

module IHaskell.Test.Hoogle ( testHoogle ) where

import           Test.Hspec
import           Text.RawString.QQ

import           IHaskell.Eval.Hoogle
-- import           Data.Text (unpack)
-- import qualified Data.Text.IO as T
preludeFmapJson :: String
preludeFmapJson = [r|
[
  {
    "url": "https://hackage.haskell.org/package/base/docs/Prelude.html#v:fmap",
    "module": {
      "url": "https://hackage.haskell.org/package/base/docs/Prelude.html",
      "name": "Prelude"
    },
    "package": {
      "url": "https://hackage.haskell.org/package/base",
      "name": "base"
    },
    "item": "<span class=name><0>fmap</0></span> :: Functor f =&gt; (a -&gt; b) -&gt; f a -&gt; f b",
    "type": "",
    "docs": ""
  }
]|]

moduleJson :: String
moduleJson = [r|
[
  {
    "url": "https://hackage.haskell.org/package/universum/docs/Universum-Functor-Fmap.html",
    "module": {},
    "package": {
      "url": "https://hackage.haskell.org/package/universum",
      "name": "universum"
    },
    "item": "<b>module</b> Universum.Functor.<span class=name><0>Fmap</0></span>",
    "type": "module",
    "docs": "This module contains useful functions to work with <a>Functor</a> type\nclass.\n"
  }
]|]

testHoogle :: Spec
testHoogle = describe "Hoogle Search" $ do
  describe "fmap search result" $ do
    let results = parseResponse preludeFmapJson :: [HoogleResult]
    it "should find 1 results" $ do
      length results `shouldBe` 1
    let (SearchResult (HoogleResponse loc signature _docUrl)) = head results
    it "should not contain html markup" $ do
      loc       `shouldBe` "https://hackage.haskell.org/package/base/docs/Prelude.html#v:fmap"
      signature `shouldBe` "fmap :: Functor f => (a -> b) -> f a -> f b"
  describe "module result" $ do
    let results = parseResponse moduleJson :: [HoogleResult]
    let (SearchResult (HoogleResponse _loc signature _docUrl)) = head results
    it "should not contain html markup" $ do
      signature `shouldBe` "module Universum.Functor.Fmap"
    it "should be renderable" $ do
      (render Plain $ head results) `shouldStartWith` "module Universum.Functor.Fmap"
      (render HTML  $ head results) `shouldStartWith` "<span class='hoogle-head'>module</span>"
