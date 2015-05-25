{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

{- | Description : Inspect type and function information and documentation.  -}
module IHaskell.Eval.Info (info) where

import           IHaskellPrelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as CBS

import           IHaskell.Eval.Evaluate (typeCleaner, Interpreter)

import           GHC
import           Outputable
import           Exception

info :: String -> Interpreter String
info name = ghandle handler $ do
  dflags <- getSessionDynFlags
  result <- exprType name
  return $ typeCleaner $ showPpr dflags result
  where
    handler :: SomeException -> Interpreter String
    handler _ = return ""
