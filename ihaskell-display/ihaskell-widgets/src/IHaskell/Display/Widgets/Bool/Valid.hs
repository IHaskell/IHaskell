{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Bool.Valid
  ( -- * The Valid Widget
    ValidWidget
    -- * Constructor
  , mkValid
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
import           IHaskell.Display.Widgets.Style.DescriptionStyle

-- | A 'ValidWidget' represents a Valid widget from IPython.html.widgets.
type ValidWidget = IPythonWidget 'ValidType

-- | Create a new output widget
mkValid :: IO ValidWidget
mkValid = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let boolState = defaultBoolWidget "ValidView" "ValidModel" layout $ StyleWidget dstyle
      validState = (ReadOutMsg =:: "") :& RNil
      widgetState = WidgetState $ boolState <+> validState

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the image widget
  return widget

instance IHaskellWidget ValidWidget where
  getCommUUID = uuid
