{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, QuasiQuotes #-}
module IHaskell.Display.Aeson where

import ClassyPrelude
import Data.Textual.Encoding
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.String.Here

import IHaskell.Display

instance IHaskellDisplay Value where
  display renderable = return [plain json, html dom]
    where 
      json = unpack $ decodeUtf8 $ encodePretty renderable
      dom = [i|<div class="highlight-code" id="javascript">${json}</div>|]
