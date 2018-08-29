{-# LANGUAGE CPP, NoImplicitPrelude, OverloadedStrings #-}

{- | Description : Inspect type and function information and documentation.  -}
module IHaskell.Eval.Info (info) where

import           IHaskellPrelude

import           IHaskell.Eval.Evaluate (typeCleaner, Interpreter)

import           GHC
import           Outputable
import           Exception

info :: String -> Interpreter String
info name = ghandle handler $ do
  dflags <- getSessionDynFlags
#if MIN_VERSION_ghc(8,2,0)
  result <- exprType TM_Inst name
#else
  result <- exprType name
#endif
  return $ typeCleaner $ showPpr dflags result
  where
    handler :: SomeException -> Interpreter String
    handler _ = return ""
