{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.String.Text
  ( -- * The Text Widget
    TextWidget
    -- * Constructor
  , mkTextWidget
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (when)
import           Data.Aeson
import           Data.IORef (newIORef)
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import           IHaskell.Display.Widgets.Layout.LayoutWidget

-- | A 'TextWidget' represents a Text widget from IPython.html.widgets.
type TextWidget = IPythonWidget 'TextType

-- | Create a new Text widget
mkTextWidget :: IO TextWidget
mkTextWidget = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout

  let widgetState = WidgetState $ defaultTextWidget "TextView" "TextModel" layout

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget TextWidget where
  getCommUUID = uuid
  -- Two possibilities: 1. content -> event -> "submit" 2. sync_data -> value -> <new_value>
  comm tw val _ = do
    case nestedObjectLookup val ["state", "value"] of
      Just (String value) -> setField' tw StringValue value >> triggerChange tw
      _                 -> pure ()
    case nestedObjectLookup val ["content", "event"] of
      Just (String event) -> when (event == "submit") $ triggerSubmit tw
      _                   -> pure ()
