{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Box.VBox
  ( -- * The VBox widget
    VBox
    -- * Constructor
  , mkVBox
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

-- | A 'VBox' represents a VBox widget from IPython.html.widgets.
type VBox = IPythonWidget 'VBoxType

-- | Create a new VBox
mkVBox :: IO VBox
mkVBox = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout

  let widgetState = WidgetState $ defaultBoxWidget "VBoxView" "VBoxModel" layout

  stateIO <- newIORef widgetState

  let vbox = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen vbox $ toJSON widgetState

  -- Return the widget
  return vbox

instance IHaskellWidget VBox where
  getCommUUID = uuid
