{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.String.Label
  ( -- * The Label Widget
    LabelWidget
    -- * Constructor
  , mkLabel
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Data.Aeson
import           Data.IORef (newIORef)

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Layout.LayoutWidget
import           IHaskell.Display.Widgets.Style.DescriptionStyle

-- | A 'LabelWidget' represents a Label widget from IPython.html.widgets.
type LabelWidget = IPythonWidget 'LabelType

-- | Create a new Label widget
mkLabel :: IO LabelWidget
mkLabel = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let widgetState = WidgetState $ defaultStringWidget "LabelView" "LabelModel" layout $ StyleWidget dstyle

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget LabelWidget where
  getCommUUID = uuid
