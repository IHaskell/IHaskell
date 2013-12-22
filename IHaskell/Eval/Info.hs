{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{- | Description : Inspect type and function information and documentation.
-}
module IHaskell.Eval.Info (
  info
  ) where

import ClassyPrelude hiding (liftIO)

import IHaskell.Eval.Evaluate (typeCleaner, Interpreter)

import GHC
import Outputable
import Exception

info :: String -> Interpreter String
info name = ghandle handler $ do
  dflags <- getSessionDynFlags
  result <- exprType name
  return $ typeCleaner $ showPpr dflags result 
  where 
    handler :: SomeException -> Interpreter String
    handler _ = return ""
