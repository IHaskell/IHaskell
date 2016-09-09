{-# LANGUAGE OverloadedStrings #-}
module Jupyter.IHaskell.Evaluate (
  Output(..),
  WidgetMessage(..),
  EvalSettings(..),
  eval,
  evalImport,
  setExtension,
  ) where

import Data.List (find)
import Control.Monad (void)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import Data.ByteString (ByteString)

import Jupyter.IHaskell.Interpreter (Interpreter, ghc)

import GHC(InteractiveImport(IIDecl), getSessionDynFlags, setSessionDynFlags)
import DynFlags(xopt_set, xopt_unset, xFlags, flagSpecFlag, flagSpecName)
import InteractiveEval (parseImportDecl, getContext, setContext)

data Output = OutputStdout Text
            | OutputStderr Text
            | OutputDisplay (Map DisplayType ByteString)
            | OutputClear
  deriving (Eq, Ord, Show)

data DisplayType = PNG
                 | JPG
                 | LaTeX
                 | Plain
                 | Javascript
  deriving (Eq, Ord, Show)

data WidgetMessage = Widget

data EvalSettings =
       EvalSettings
         { evalAllowWidgets :: Bool
         , evalAllowRichDisplays :: Bool
         , evalGetStdin :: Maybe (IO Text)
         , evalHandleOutput :: Output -> IO ()
         , evalHandleWidgetComm :: Maybe (WidgetMessage -> IO ())
         }

eval :: EvalSettings -> Text -> Interpreter Output
eval = undefined

evalImport :: Text -> Interpreter ()
evalImport text = ghc $ do
  decl <- parseImportDecl $ T.unpack text
  ctx <- getContext
  setContext $ IIDecl decl : ctx

setExtension :: Text -> Interpreter ()
setExtension ext = ghc $ do
  flags <- getSessionDynFlags
  case find flagMatches xFlags of
    Just fs -> void $ setSessionDynFlags $ xopt_set flags (flagSpecFlag fs)
    -- If it doesn't match an extension name, try matching against disabling an extension.
    Nothing ->
      case find flagMatchesNo xFlags of
        Just fs -> void $ setSessionDynFlags $ xopt_unset flags (flagSpecFlag fs)
        Nothing -> fail $ "No such extension: " ++ T.unpack ext

  where
    -- Check if a FlagSpec matches an extension name.
    flagMatches fs = ext == T.pack (flagSpecName fs)

    -- Check if a FlagSpec matches "No<ExtensionName>". In that case, we disable the extension.
    flagMatchesNo fs = ext == T.pack ("No" ++ flagSpecName fs)
