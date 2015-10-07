{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Float.BoundedFloat.FloatProgress (
-- * The FloatProgress Widget
FloatProgress, 
               -- * Constructor
               mkFloatProgress) where

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

-- | 'FloatProgress' represents an FloatProgress widget from IPython.html.widgets.
type FloatProgress = IPythonWidget FloatProgressType

-- | Create a new widget
mkFloatProgress :: IO FloatProgress
mkFloatProgress = do
  -- Default properties, with a random uuid
  uuid <- U.random

  let boundedFloatAttrs = defaultBoundedFloatWidget "ProgressView"
      progressAttrs = (Orientation =:: HorizontalOrientation)
                      :& (BarStyle =:: DefaultBar)
                      :& RNil
      widgetState = WidgetState $ boundedFloatAttrs <+> progressAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellDisplay FloatProgress where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget FloatProgress where
  getCommUUID = uuid
