{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Box.GridBox
  ( -- * The GridBox widget
    GridBox
    -- * Constructor
  , mkGridBox
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

-- | A 'GridBox' represents a GridBox widget from IPython.html.widgets.
type GridBox = IPythonWidget 'GridBoxType

-- | Create a new GridBox
mkGridBox :: IO GridBox
mkGridBox = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout

  let widgetState = WidgetState $ defaultBoxWidget "GridBoxView" "GridBoxModel" layout

  stateIO <- newIORef widgetState

  let gridBox = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen gridBox $ toJSON widgetState

  -- Return the widget
  return gridBox

instance IHaskellWidget GridBox where
  getCommUUID = uuid
