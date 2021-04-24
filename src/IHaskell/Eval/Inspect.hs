{-# LANGUAGE NoImplicitPrelude, CPP, OverloadedStrings, DoAndIfThenElse, FlexibleContexts #-}

{- |
Description:    Generates inspections when asked for by the frontend.

-}
module IHaskell.Eval.Inspect (inspect) where

import           IHaskellPrelude

import qualified Prelude as P

import           Data.List.Split (splitOn)

#if MIN_VERSION_ghc(9,0,0)
import qualified Control.Monad.Catch as MC
#else
import           Exception (ghandle)
#endif

import           IHaskell.Eval.Evaluate (Interpreter)
import           IHaskell.Display
import           IHaskell.Eval.Util (getType)

-- | Characters used in Haskell operators.
operatorChars :: String
operatorChars = "!#$%&*+./<=>?@\\^|-~:"

-- | Whitespace characters.
whitespace :: String
whitespace = " \t\n"

-- | Compute the identifier that is being queried.
getIdentifier :: String -> Int -> String
getIdentifier code _pos = identifier
  where
    chunks = splitOn whitespace code
    lastChunk = P.last chunks :: String
    identifier =
      if all (`elem` operatorChars) lastChunk
        then "(" ++ lastChunk ++ ")"
        else lastChunk

inspect :: String -- ^ Code in the cell
        -> Int    -- ^ Cursor position in the cell
        -> Interpreter (Maybe Display)
inspect code pos = do
  let identifier = getIdentifier code pos
      handler :: SomeException -> Interpreter (Maybe a)
      handler _ = return Nothing
#if MIN_VERSION_ghc(9,0,0)
  response <- MC.handle handler (Just <$> getType identifier)
#else
  response <- ghandle handler (Just <$> getType identifier)
#endif
  let prefix = identifier ++ " :: "
      fmt str = Display [plain $ prefix ++ str]
  return $ fmt <$> response
