{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Button
  ( -- * The Button Widget
    Button
    -- * Create a new button
  , mkButton
    -- * Button style
  , ButtonStyle
    -- * Create a new button style
  , mkButtonStyle
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

-- | A 'ButtonStyle' represents a Button Style from IPython.html.widgets.
type ButtonStyle = IPythonWidget 'ButtonStyleType

-- | Create a new button style
mkButtonStyle :: IO ButtonStyle
mkButtonStyle = do
  wid <- U.random

  let stl = defaultStyleWidget "ButtonStyleModel"
      but = (ButtonColor =:: Nothing)
            :& (FontWeight =:: DefaultWeight)
            :& RNil
      btnStlState = WidgetState (stl <+> but)

  stateIO <- newIORef btnStlState

  let style = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen style $ toJSON btnStlState

  -- Return the style widget
  return style

instance IHaskellWidget ButtonStyle where
  getCommUUID = uuid

-- | A 'Button' represents a Button from IPython.html.widgets.
type Button = IPythonWidget 'ButtonType

-- | Create a new button
mkButton :: IO Button
mkButton = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout

  style <- mkButtonStyle

  let ddw = defaultDescriptionWidget "ButtonView" "ButtonModel" layout
      but = (Disabled =:: False)
            :& (Icon =:: "")
            :& (ButtonStyle =:: DefaultButton)
            :& (StyleButton =:: style)
            :& (ClickHandler =:: return ())
            :& RNil
      buttonState = WidgetState (ddw <+> but)

  stateIO <- newIORef buttonState

  let button = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen button $ toJSON buttonState

  -- Return the button widget
  return button

instance IHaskellWidget Button where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["content", "event"] of
      Just (String "click") -> triggerClick widget
      _ -> pure ()
