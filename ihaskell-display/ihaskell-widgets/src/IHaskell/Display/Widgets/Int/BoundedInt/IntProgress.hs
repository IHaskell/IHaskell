{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Int.BoundedInt.IntProgress
  ( -- * The IntProgress Widget
    IntProgress
    -- * Constructor
  , mkIntProgress
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

-- | 'IntProgress' represents an IntProgress widget from IPython.html.widgets.
type IntProgress = IPythonWidget 'IntProgressType

-- | Create a new widget
mkIntProgress :: IO IntProgress
mkIntProgress = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let boundedIntAttrs = defaultBoundedIntWidget "ProgressView" "IntProgressModel" layout $ StyleWidget dstyle
      progressAttrs = (Orientation =:: HorizontalOrientation)
                      :& (BarStyle =:: DefaultBar)
                      :& RNil
      widgetState = WidgetState $ boundedIntAttrs <+> progressAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget IntProgress where
  getCommUUID = uuid
