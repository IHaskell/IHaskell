{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.Selection.SelectMultiple
  ( -- * The SelectMultiple Widget
    SelectMultiple
    -- * Constructor
  , mkSelectMultiple
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (void)
import           Data.Aeson
import           Data.IORef (newIORef)
import qualified Data.Scientific as Sci
import qualified Data.Vector as V

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | A 'SelectMultiple' represents a SelectMultiple widget from IPython.html.widgets.
type SelectMultiple = IPythonWidget 'SelectMultipleType

-- | Create a new SelectMultiple widget
mkSelectMultiple :: IO SelectMultiple
mkSelectMultiple = do
  -- Default properties, with a random uuid
  wid <- U.random
  let widgetState = WidgetState $ defaultMultipleSelectionWidget "SelectMultipleView" "SelectMultipleModel"

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget SelectMultiple where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["state", "index"] of
      Just (Array indices) -> do
        let indicesList = map (\(Number x) -> Sci.coefficient x) $ V.toList indices
        void $ setField' widget Indices indicesList
        triggerSelection widget
      _ -> pure ()
