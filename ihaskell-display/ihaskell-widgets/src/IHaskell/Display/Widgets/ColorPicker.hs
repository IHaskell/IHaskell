{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.ColorPicker
  ( -- * The ColorPicker Widget
    ColorPicker
    -- * Create a new ColorPicker
  , mkColorPicker
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

-- | A 'ColorPicker' represents a ColorPicker from IPython.html.widgets.
type ColorPicker = IPythonWidget 'ColorPickerType

-- | Create a new ColorPicker
mkColorPicker :: IO ColorPicker
mkColorPicker = do
  -- Default properties, with a random uuid
  wid <- U.random

  let ddw = defaultDescriptionWidget "ColorPickerView" "ColorPickerModel"
      color = (StringValue =:: "black")
              :& (Concise =:: False)
              :& (Disabled =:: False)
              :& RNil
      colorPickerState = WidgetState (ddw <+> color)

  stateIO <- newIORef colorPickerState

  let colorPicker = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen colorPicker $ toJSON colorPickerState

  -- Return the ColorPicker widget
  return colorPicker

instance IHaskellWidget ColorPicker where
  getCommUUID = uuid
