{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.String.HTMLMath
  ( -- * The HTMLMath Widget
    HTMLMathWidget
    -- * Constructor
  , mkHTMLMath
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
import           IHaskell.Display.Widgets.Style.DescriptionStyle

-- | A 'HTMLMathWidget' represents a HTML Math widget from IPython.html.widgets.
type HTMLMathWidget = IPythonWidget 'HTMLMathType

-- | Create a new HTML widget
mkHTMLMath :: IO HTMLMathWidget
mkHTMLMath = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let widgetState = WidgetState $ defaultStringWidget "HTMLMathView" "HTMLMathModel" layout $ StyleWidget dstyle

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget HTMLMathWidget where
  getCommUUID = uuid
