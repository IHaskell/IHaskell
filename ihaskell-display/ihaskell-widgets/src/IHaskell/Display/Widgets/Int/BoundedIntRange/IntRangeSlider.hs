{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Int.BoundedIntRange.IntRangeSlider (
-- * The IntRangeSlider Widget
IntRangeSlider, 
                -- * Constructor
                mkIntRangeSlider) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

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

-- | 'IntRangeSlider' represents an IntRangeSlider widget from IPython.html.widgets.
type IntRangeSlider = IPythonWidget IntRangeSliderType

-- | Create a new widget
mkIntRangeSlider :: IO IntRangeSlider
mkIntRangeSlider = do
  -- Default properties, with a random uuid
  uuid <- U.random

  let boundedIntAttrs = defaultBoundedIntRangeWidget "IntSliderView"
      sliderAttrs = (Orientation =:: HorizontalOrientation)
                    :& (ShowRange =:: True)
                    :& (ReadOut =:: True)
                    :& (SliderColor =:: "")
                    :& RNil
      widgetState = WidgetState $ boundedIntAttrs <+> sliderAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellDisplay IntRangeSlider where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget IntRangeSlider where
  getCommUUID = uuid
  comm widget (Object dict1) _ = do
    let key1 = "sync_data" :: Text
        key2 = "value" :: Text
        Just (Object dict2) = HM.lookup key1 dict1
        Just (Array values) = HM.lookup key2 dict2
        [x, y] = map (\(Number x) -> Sci.coefficient x) $ V.toList values
    setField' widget IntPairValue (x, y)
    triggerChange widget
