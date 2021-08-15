{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Controller.ControllerButton
  ( -- * The ControllerButton Widget
    ControllerButton
    -- * Constructor
  , mkControllerButton
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

-- | 'ControllerButton' represents an ControllerButton widget from IPython.html.widgets.
type ControllerButton = IPythonWidget 'ControllerButtonType

-- | Create a new widget
mkControllerButton :: IO ControllerButton
mkControllerButton = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout

  let domAttrs = defaultCoreWidget <+> defaultDOMWidget "ControllerButtonView" "ControllerButtonModel" layout
      btnAttrs = (FloatValue =:! 0.0)
                 :& (Pressed =:! False)
                 :& (ChangeHandler =:: pure ())
                 :& RNil
      widgetState = WidgetState $ domAttrs <+> btnAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget ControllerButton where
  getCommUUID = uuid
