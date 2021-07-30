{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Int.BoundedIntRange.IntRangeSlider
  ( -- * The IntRangeSlider Widget
    IntRangeSlider
    -- * Constructor
  , mkIntRangeSlider
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (void)
import           Data.Aeson
import           Data.IORef (newIORef)
import qualified Data.Scientific as Sci
import qualified Data.Vector as V
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import           IHaskell.Display.Widgets.Layout.LayoutWidget
import           IHaskell.Display.Widgets.Style.DescriptionStyle

-- | 'IntRangeSlider' represents an IntRangeSlider widget from IPython.html.widgets.
type IntRangeSlider = IPythonWidget 'IntRangeSliderType

-- | Create a new widget
mkIntRangeSlider :: IO IntRangeSlider
mkIntRangeSlider = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let boundedIntAttrs = defaultBoundedIntRangeWidget "IntRangeSliderView" "IntRangeSliderModel" layout $ StyleWidget dstyle
      sliderAttrs = (StepInt =:: Just 1)
                    :& (Orientation =:: HorizontalOrientation)
                    :& (ReadOut =:: True)
                    :& (ReadOutFormat =:: "d")
                    :& (ContinuousUpdate =:: True)
                    :& (Disabled =:: False)
                    :& RNil
      widgetState = WidgetState $ boundedIntAttrs <+> sliderAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget IntRangeSlider where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["state", "value"] of
      Just (Array values) ->
        case map (\(Number x) -> Sci.coefficient x) $ V.toList values of
          [x, y] -> do
            void $ setField' widget IntPairValue (x, y)
            triggerChange widget
          _ -> pure ()
      _ -> pure ()
