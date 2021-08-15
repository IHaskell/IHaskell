{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Float.BoundedFloat.FloatSlider
  ( -- * The FloatSlider Widget
    FloatSlider
    -- * Constructor
  , mkFloatSlider
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (void)
import           Data.Aeson
import           Data.IORef (newIORef)
import qualified Data.Scientific as Sci
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import           IHaskell.Display.Widgets.Layout.LayoutWidget
import           IHaskell.Display.Widgets.Style.DescriptionStyle

-- | 'FloatSlider' represents an FloatSlider widget from IPython.html.widgets.
type FloatSlider = IPythonWidget 'FloatSliderType

-- | Create a new widget
mkFloatSlider :: IO FloatSlider
mkFloatSlider = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let boundedFloatAttrs = defaultBoundedFloatWidget "FloatSliderView" "FloatSliderModel" layout $ StyleWidget dstyle
      sliderAttrs = (StepFloat =:: Just 0.1)
                    :& (Orientation =:: HorizontalOrientation)
                    :& (ReadOut =:: True)
                    :& (ReadOutFormat =:: ".2f")
                    :& (ContinuousUpdate =:: True)
                    :& (Disabled =:: False)
                    :& RNil
      widgetState = WidgetState $ boundedFloatAttrs <+> sliderAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget FloatSlider where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["state", "value"] of
      Just (Number value) -> do
        void $ setField' widget FloatValue (Sci.toRealFloat value)
        triggerChange widget
      _ -> pure ()
