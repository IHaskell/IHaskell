{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.Selection.SelectionRangeSlider
  ( -- * The SelectionRangeSlider Widget
    SelectionRangeSlider
    -- * Constructor
  , mkSelectionRangeSlider
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (void)
import           Data.Aeson
import           Data.IORef (newIORef)
import qualified Data.Scientific as Sci
import qualified Data.Vector as V
import           Data.Vinyl (Rec(..), (<+>), rput)

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import           IHaskell.Display.Widgets.Layout.LayoutWidget
import           IHaskell.Display.Widgets.Style.DescriptionStyle

-- | A 'SelectionRangeSlider' represents a SelectionSlider widget from IPyhon.widgets
type SelectionRangeSlider = IPythonWidget 'SelectionRangeSliderType

-- | Create a new SelectionRangeSlider widget
mkSelectionRangeSlider :: IO SelectionRangeSlider
mkSelectionRangeSlider = do
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let selectionAttrs = defaultMultipleSelectionWidget "SelectionRangeSliderView" "SelectionRangeSliderModel" layout $ StyleWidget dstyle
      selectionRangeSliderAttrs = (Orientation =:: HorizontalOrientation)
                                  :& (ReadOut =:: True)
                                  :& (ContinuousUpdate =:: True)
                                  :& RNil
      widgetState = WidgetState $ rput (Indices =:. ([0,0], rangeSliderVerification)) $ selectionAttrs <+> selectionRangeSliderAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the created widget
  return widget

instance IHaskellWidget SelectionRangeSlider where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["state", "index"] of
      Just (Array indices) -> do
        let indicesList = map (\(Number x) -> Sci.coefficient x) $ V.toList indices
        void $ setField' widget Indices indicesList
        triggerSelection widget
      _ -> pure ()