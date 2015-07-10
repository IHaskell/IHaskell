{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Float.BoundedFloat.FloatProgress (
  -- * The FloatProgress Widget
  FloatProgressWidget,
  -- * Constructor
  mkFloatProgressWidget) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Exception (throw, ArithException (LossOfPrecision))
import           Control.Monad (when, join)
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.IORef (newIORef)
import qualified Data.Scientific as Sci
import           Data.Text (Text)
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | 'FloatProgressWidget' represents an FloatProgress widget from IPython.html.widgets.
type FloatProgressWidget = IPythonWidget FloatProgressType

-- | Create a new widget
mkFloatProgressWidget :: IO FloatProgressWidget
mkFloatProgressWidget = do
  -- Default properties, with a random uuid
  uuid <- U.random

  let boundedFloatAttrs = defaultBoundedFloatWidget "ProgressView"
      progressAttrs = (SBarStyle =:: DefaultBar) :& RNil
      widgetState = WidgetState $ boundedFloatAttrs <+> progressAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO
      initData = object
                   [ "model_name" .= str "WidgetModel"
                   , "widget_class" .= str "IPython.FloatProgress"
                   ]

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget initData $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellDisplay FloatProgressWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget FloatProgressWidget where
  getCommUUID = uuid
