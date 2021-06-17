{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.Selection.Select
  ( -- * The Select Widget
    Select
    -- * Constructor
  , mkSelect
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (void)
import           Data.Aeson
import           Data.IORef (newIORef)

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | A 'Select' represents a Select widget from IPython.html.widgets.
type Select = IPythonWidget 'SelectType

-- | Create a new Select widget
mkSelect :: IO Select
mkSelect = do
  -- Default properties, with a random uuid
  wid <- U.random
  let widgetState = WidgetState $ defaultSelectionWidget "SelectView" "SelectModel"

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget Select where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["sync_data", "selected_label"] of
      Just (String label) -> do
        opts <- getField widget Options
        case opts of
          OptionLabels _ -> do
            void $ setField' widget SelectedLabel label
            void $ setField' widget SelectedValue label
          OptionDict ps ->
            case lookup label ps of
              Nothing -> pure ()
              Just value -> do
                void $ setField' widget SelectedLabel label
                void $ setField' widget SelectedValue value
        triggerSelection widget
      _ -> pure ()
