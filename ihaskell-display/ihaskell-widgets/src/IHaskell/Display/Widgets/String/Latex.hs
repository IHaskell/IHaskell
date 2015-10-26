{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.String.Latex (
-- * The Latex Widget
LatexWidget, 
             -- * Constructor
             mkLatexWidget) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Data.Aeson
import           Data.IORef (newIORef)

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types

-- | A 'LatexWidget' represents a Latex widget from IPython.html.widgets.
type LatexWidget = IPythonWidget LatexType

-- | Create a new Latex widget
mkLatexWidget :: IO LatexWidget
mkLatexWidget = do
  -- Default properties, with a random uuid
  uuid <- U.random
  let widgetState = WidgetState $ defaultStringWidget "LatexView"

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellDisplay LatexWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget LatexWidget where
  getCommUUID = uuid
