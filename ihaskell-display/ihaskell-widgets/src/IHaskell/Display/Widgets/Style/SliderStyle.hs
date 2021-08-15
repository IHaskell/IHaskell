{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Style.SliderStyle
  ( -- * Slider style
    SliderStyle
    -- * Create a new slider style
  , mkSliderStyle
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

-- | A 'SliderStyle' represents a Button Style from IPython.html.widgets.
type SliderStyle = IPythonWidget 'SliderStyleType

-- | Create a new button style
mkSliderStyle :: IO SliderStyle
mkSliderStyle = do
  wid <- U.random

  let stl = defaultDescriptionStyleWidget "SliderStyleModel"
      but = (HandleColor =:: Nothing)
            :& RNil
      btnStlState = WidgetState (stl <+> but)

  stateIO <- newIORef btnStlState

  let style = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen style $ toJSON btnStlState

  -- Return the style widget
  return style

instance IHaskellWidget SliderStyle where
  getCommUUID = uuid
