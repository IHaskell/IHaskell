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
    case nestedObjectLookup val ["sync_data", "selected_labels"] of
      Just (Array labels) -> do
        let labelList = map (\(String x) -> x) $ V.toList labels
        opts <- getField widget Options
        case opts of
          OptionLabels _ -> do
            void $ setField' widget SelectedLabels labelList
            void $ setField' widget SelectedValues labelList
          OptionDict ps ->
            case mapM (`lookup` ps) labelList of
              Nothing -> pure ()
              Just valueList -> do
                void $ setField' widget SelectedLabels labelList
                void $ setField' widget SelectedValues valueList
        triggerSelection widget
      _ -> pure ()
