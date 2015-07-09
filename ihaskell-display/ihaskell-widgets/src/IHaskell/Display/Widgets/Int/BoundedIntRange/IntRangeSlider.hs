{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Int.BoundedIntRange.IntRangeSlider (
  -- * The IntRangeSlider Widget
  IntRangeSliderWidget,
  -- * Constructor
  mkIntRangeSliderWidget) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Exception (throw, ArithException (LossOfPrecision))
import           Control.Monad (when, join)
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.IORef (newIORef)
import qualified Data.Scientific as Sci
import           Data.Text (Text)
import qualified Data.Vector as V
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | 'IntRangeSliderWidget' represents an IntRangeSlider widget from IPython.html.widgets.
type IntRangeSliderWidget = IPythonWidget IntRangeSliderType

-- | Create a new widget
mkIntRangeSliderWidget :: IO IntRangeSliderWidget
mkIntRangeSliderWidget = do
  -- Default properties, with a random uuid
  uuid <- U.random

  let boundedIntAttrs = defaultBoundedIntRangeWidget "IntSliderView"
      sliderAttrs = (SOrientation =:: HorizontalOrientation)
                 :& (SShowRange =:: True)
                 :& (SReadOut =:: True)
                 :& (SSliderColor =:: "")
                 :& RNil
      widgetState = WidgetState $ boundedIntAttrs <+> sliderAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO
      initData = object
                   [ "model_name" .= str "WidgetModel"
                   , "widget_class" .= str "IPython.IntRangeSlider"
                   ]

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget initData $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellDisplay IntRangeSliderWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget IntRangeSliderWidget where
  getCommUUID = uuid
  comm widget (Object dict1) _ = do
    let key1 = "sync_data" :: Text
        key2 = "value" :: Text
        Just (Object dict2) = HM.lookup key1 dict1
        Just (Array values) = HM.lookup key2 dict2
        [x, y] = map (\(Number x) -> Sci.coefficient x) $ V.toList values
    setField' widget SIntPairValue (x, y)
