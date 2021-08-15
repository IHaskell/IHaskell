{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Controller.ControllerAxis
  ( -- * The ControllerAxis Widget
    ControllerAxis
    -- * Constructor
  , mkControllerAxis
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Data.Aeson
import           Data.IORef (newIORef)
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import           IHaskell.Display.Widgets.Layout.LayoutWidget

-- | 'ControllerAxis' represents an ControllerAxis widget from IPython.html.widgets.
type ControllerAxis = IPythonWidget 'ControllerAxisType

-- | Create a new widget
mkControllerAxis :: IO ControllerAxis
mkControllerAxis = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout

  let domAttrs = defaultCoreWidget <+> defaultDOMWidget "ControllerAxisView" "ControllerAxisModel" layout
      axisAttrs = (FloatValue =:! 0.0)
                  :& (ChangeHandler =:: pure ())
                  :& RNil
      widgetState = WidgetState $ domAttrs <+> axisAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget ControllerAxis where
  getCommUUID = uuid
