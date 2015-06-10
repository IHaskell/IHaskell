{-# LANGUAGE TypeSynonymInstances, QuasiQuotes #-}

module IHaskell.Display.Aeson () where

import           Data.Text as T
import           Data.ByteString.Lazy as LBS
import           Data.Text.Encoding as E

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.String.Here

import           IHaskell.Display

instance IHaskellDisplay Value where
  display renderable = return $ Display [plain json, html dom]
    where
      json = T.unpack $ E.decodeUtf8 $ LBS.toStrict $ encodePretty renderable
      dom = [i|<div class="highlight-code" id="javascript">${json}</div>|]