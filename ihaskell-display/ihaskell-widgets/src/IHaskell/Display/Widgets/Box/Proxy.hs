{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Box.Proxy (
-- * The Proxy widget
ProxyWidget, 
             -- * Constructor
             mkProxyWidget) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Data.Aeson
import           Data.IORef (newIORef)
import           Data.Vinyl (Rec(..), (<+>))
import           Data.Vinyl.Lens (rput)

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | A 'Box' represents a Box widget from IPython.html.widgets.
type ProxyWidget = IPythonWidget ProxyType

-- | Create a new box
mkProxyWidget :: IO ProxyWidget
mkProxyWidget = do
  -- Default properties, with a random uuid
  uuid <- U.random

  let widgetClassState = defaultWidget "ProxyView"
      baseState = rput (ModelName =:: "ProxyModel") widgetClassState
      proxyState = (Child =:: Nothing) :& RNil
      widgetState = WidgetState $ baseState <+> proxyState

  stateIO <- newIORef widgetState

  let proxy = IPythonWidget uuid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen proxy $ toJSON widgetState

  -- Return the widget
  return proxy

instance IHaskellDisplay ProxyWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget ProxyWidget where
  getCommUUID = uuid
