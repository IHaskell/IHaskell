{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Float.BoundedFloat.BoundedFloatText
  ( -- * The BoundedFloatText Widget
    BoundedFloatText
    -- * Constructor
  , mkBoundedFloatText
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

-- | 'BoundedFloatText' represents an BoundedFloatText widget from IPython.html.widgets.
type BoundedFloatText = IPythonWidget 'BoundedFloatTextType

-- | Create a new widget
mkBoundedFloatText :: IO BoundedFloatText
mkBoundedFloatText = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let boundedFloatAttrs = defaultBoundedFloatWidget "FloatTextView" "BoundedFloatTextModel" layout $ StyleWidget dstyle
      textAttrs = (Disabled =:: False)
                  :& (ContinuousUpdate =:: False)
                  :& (StepFloat =:: Nothing)
                  :& RNil
      widgetState = WidgetState $ boundedFloatAttrs <+> textAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget BoundedFloatText where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["state", "value"] of
      Just (Number value) -> do
        void $ setField' widget FloatValue (Sci.toRealFloat value)
        triggerChange widget
      _ -> pure ()
