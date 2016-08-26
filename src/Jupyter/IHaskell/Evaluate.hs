{-# LANGUAGE OverloadedStrings #-}
module Jupyter.IHaskell.Evaluate (
  Output(..),
  WidgetMessage(..),
  EvalSettings(..),
  eval,
  evalImport,
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import Data.ByteString (ByteString)

import Jupyter.IHaskell.Interpreter (Interpreter, ghc)

import GHC(InteractiveImport(IIDecl))
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
