{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.String.TextArea (
    -- * The TextArea Widget
    TextAreaWidget,
    -- * Constructor
    mkTextAreaWidget,
    ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (when, join)
import           Data.Aeson
import           Data.IORef (newIORef)
import           Data.Text (Text)
import           Data.Vinyl (Rec (..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types

-- | A 'TextAreaWidget' represents a Textarea widget from IPython.html.widgets.
type TextAreaWidget = IPythonWidget TextAreaType

-- | Create a new TextArea widget
mkTextAreaWidget :: IO TextAreaWidget
mkTextAreaWidget = do
  -- Default properties, with a random uuid
  uuid <- U.random
  let widgetState = WidgetState $ defaultStringWidget "TextareaView"

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO
      initData = object ["model_name" .= str "WidgetModel", "widget_class" .= str "IPython.Textarea"]

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget initData $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellDisplay TextAreaWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget TextAreaWidget where
  getCommUUID = uuid
