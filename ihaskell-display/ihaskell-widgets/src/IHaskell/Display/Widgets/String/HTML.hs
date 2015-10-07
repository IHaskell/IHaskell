{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.String.HTML (
-- * The HTML Widget
HTMLWidget, 
            -- * Constructor
            mkHTMLWidget) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Data.Aeson
import           Data.IORef (newIORef)

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types

-- | A 'HTMLWidget' represents a HTML widget from IPython.html.widgets.
type HTMLWidget = IPythonWidget HTMLType

-- | Create a new HTML widget
mkHTMLWidget :: IO HTMLWidget
mkHTMLWidget = do
  -- Default properties, with a random uuid
  uuid <- U.random
  let widgetState = WidgetState $ defaultStringWidget "HTMLView"

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellDisplay HTMLWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget HTMLWidget where
  getCommUUID = uuid
