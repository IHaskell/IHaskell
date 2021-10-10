{-# LANGUAGE CPP, NoImplicitPrelude, OverloadedStrings #-}

{- | Description : Inspect type and function information and documentation.  -}
module IHaskell.Eval.Info (info) where

import           IHaskellPrelude

import           IHaskell.Eval.Evaluate (typeCleaner, Interpreter)

import           GHC
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Ppr
import           Control.Monad.Catch (handle)
#elif MIN_VERSION_ghc(9,0,0)
import           GHC.Utils.Outputable
import           Control.Monad.Catch (handle)
#else
import           Outputable
import           Exception
#endif

info :: String -> Interpreter String
#if MIN_VERSION_ghc(9,0,0)
info name = handle handler $ do
#else
info name = ghandle handler $ do
#endif
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
