{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Int.BoundedInt.IntSlider (
-- * The IntSlider Widget
IntSlider, 
           -- * Constructor
           mkIntSlider) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

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

-- | 'IntSlider' represents an IntSlider widget from IPython.html.widgets.
type IntSlider = IPythonWidget IntSliderType

-- | Create a new widget
mkIntSlider :: IO IntSlider
mkIntSlider = do
  -- Default properties, with a random uuid
  uuid <- U.random

  let boundedIntAttrs = defaultBoundedIntWidget "IntSliderView"
      sliderAttrs = (Orientation =:: HorizontalOrientation)
                    :& (ShowRange =:: False)
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

instance IHaskellDisplay IntSlider where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget IntSlider where
  getCommUUID = uuid
  comm widget (Object dict1) _ = do
    let key1 = "sync_data" :: Text
        key2 = "value" :: Text
        Just (Object dict2) = HM.lookup key1 dict1
        Just (Number value) = HM.lookup key2 dict2
    setField' widget IntValue (Sci.coefficient value)
    triggerChange widget
