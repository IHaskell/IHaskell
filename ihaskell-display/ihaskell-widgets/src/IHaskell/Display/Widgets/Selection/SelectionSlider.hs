{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.Selection.SelectionSlider
  ( -- * The SelectionSlider Widget
    SelectionSlider
    -- * Constructor
  , mkSelectionSlider
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

-- | A 'SelectionSlider' represents a SelectionSlider widget from IPyhon.widgets
type SelectionSlider = IPythonWidget 'SelectionSliderType

-- | Create a new SelectionSLider widget
mkSelectionSlider :: IO SelectionSlider
mkSelectionSlider = do
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let selectionAttrs = defaultSelectionNonemptyWidget "SelectionSliderView" "SelectionSliderModel" layout $ StyleWidget dstyle
      selectionSliderAttrs = (Orientation =:: HorizontalOrientation)
                             :& (ReadOut =:: True)
                             :& (ContinuousUpdate =:: True)
                             :& RNil
      widgetState = WidgetState $ selectionAttrs <+> selectionSliderAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the created widget
  return widget

instance IHaskellWidget SelectionSlider where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["state", "index"] of
      Just (Number index) -> do
        void $ setField' widget Index (Sci.coefficient index)
        triggerSelection widget
      _ -> pure ()