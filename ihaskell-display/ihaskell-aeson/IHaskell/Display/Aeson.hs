{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Aeson () where

import           Data.Text as T
import           Data.ByteString.Lazy as LBS
import           Data.Text.Encoding as E

import           Data.Aeson
import           Data.Aeson.Encode.Pretty

import           IHaskell.Display

instance IHaskellDisplay Value where
  display renderable = return $ Display [plain json]
    where
      json = T.unpack $ E.decodeUtf8 $ LBS.toStrict $ encodePretty renderable
