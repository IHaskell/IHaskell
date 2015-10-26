{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Box.PlaceProxy (
-- * The PlaceProxy widget
PlaceProxy, 
            -- * Constructor
            mkPlaceProxy) where

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
type PlaceProxy = IPythonWidget PlaceProxyType

-- | Create a new box
mkPlaceProxy :: IO PlaceProxy
mkPlaceProxy = do
  -- Default properties, with a random uuid
  uuid <- U.random

  let widgetClassState = defaultWidget "PlaceProxyView"
      baseState = rput (ModelName =:: "ProxyModel") widgetClassState
      proxyState = (Child =:: Nothing) :& (Selector =:: "") :& RNil
      widgetState = WidgetState $ baseState <+> proxyState

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellDisplay PlaceProxy where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget PlaceProxy where
  getCommUUID = uuid
