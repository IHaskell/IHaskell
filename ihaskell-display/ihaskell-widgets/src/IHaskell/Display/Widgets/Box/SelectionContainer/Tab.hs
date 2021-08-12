{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Box.SelectionContainer.Tab
  ( -- * The Tab widget
    TabWidget
    -- * Constructor
  , mkTab
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (void)

import           Data.Aeson
import           Data.IORef (newIORef)
import qualified Data.Scientific as Sci

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import           IHaskell.Display.Widgets.Layout.LayoutWidget

-- | A 'TabWidget' represents a Tab widget from IPython.html.widgets.
type TabWidget = IPythonWidget 'TabType

-- | Create a new box
mkTab :: IO TabWidget
mkTab = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout

  let widgetState = WidgetState $ defaultSelectionContainerWidget "TabView" "TabModel" layout

  stateIO <- newIORef widgetState

  let box = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen box $ toJSON widgetState

  -- Return the widget
  return box

instance IHaskellWidget TabWidget where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["state", "selected_index"] of
      Just (Number num) -> do
        void $ setField' widget SelectedIndex $ Just (Sci.coefficient num)
        triggerChange widget
      Just Null -> do
        void $ setField' widget SelectedIndex Nothing
        triggerChange widget
      _ -> pure ()
