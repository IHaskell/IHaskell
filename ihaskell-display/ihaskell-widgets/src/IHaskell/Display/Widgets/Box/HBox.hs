{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Box.HBox
  ( -- * The HBox widget
    HBox
    -- * Constructor
  , mkHBox
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

-- | A 'HBox' represents a HBox widget from IPython.html.widgets.
type HBox = IPythonWidget HBoxType

-- | Create a new HBox
mkHBox :: IO HBox
mkHBox = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout

  let widgetState = WidgetState $ defaultBoxWidget "HBoxView" "HBoxModel" layout

  stateIO <- newIORef widgetState

  let hBox = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen hBox $ toJSON widgetState

  -- Return the widget
  return hBox

instance IHaskellWidget HBox where
  getCommUUID = uuid
