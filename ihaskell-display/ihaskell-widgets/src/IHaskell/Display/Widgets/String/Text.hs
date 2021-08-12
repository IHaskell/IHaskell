{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.String.Text
  ( -- * The Text Widget
    TextWidget
    -- * Constructor
  , mkText
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (when)
import           Data.Aeson
import           Data.IORef (newIORef)

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import           IHaskell.Display.Widgets.Layout.LayoutWidget
import           IHaskell.Display.Widgets.Style.DescriptionStyle

-- | A 'TextWidget' represents a Text widget from IPython.html.widgets.
type TextWidget = IPythonWidget 'TextType

-- | Create a new Text widget
mkText :: IO TextWidget
mkText = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let widgetState = WidgetState $ defaultTextWidget "TextView" "TextModel" layout $ StyleWidget dstyle

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget TextWidget where
  getCommUUID = uuid
  -- Two possibilities: 1. content -> event -> "submit" 2. state -> value -> <new_value>
  comm tw val _ = do
    case nestedObjectLookup val ["state", "value"] of
      Just (String value) -> setField' tw StringValue value >> triggerChange tw
      _                 -> pure ()
    case nestedObjectLookup val ["content", "event"] of
      Just (String event) -> when (event == "submit") $ triggerSubmit tw
      _                   -> pure ()
