{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Style.ButtonStyle
  ( -- * Button style
    ButtonStyle
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
import           IHaskell.Display.Widgets.Common as C

-- | A 'ButtonStyle' represents a Button Style from IPython.html.widgets.
type ButtonStyle = IPythonWidget ButtonStyleType

-- | Create a new button style
mkButtonStyle :: IO ButtonStyle
mkButtonStyle = do
  wid <- U.random

  let stl = defaultStyleWidget "ButtonStyleModel"
      but = (F @ButtonColor =:: Nothing)
            :& (F @FontFamily =:: Nothing)
            :& (F @FontSize =:: Nothing)
            :& (F @FontStyle =:: Nothing)
            :& (F @FontVariant =:: Nothing)
            :& (F @FontWeight =:: DefaultWeight)
            :& (F @TextColor =:: Nothing)
            :& (F @TextDecoration =:: Nothing)
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
