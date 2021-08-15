{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Int.BoundedInt.BoundedIntText
  ( -- * The BoundedIntText Widget
    BoundedIntText
    -- * Constructor
  , mkBoundedIntText
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

-- | 'BoundedIntText' represents an BoundedIntText widget from IPython.html.widgets.
type BoundedIntText = IPythonWidget 'BoundedIntTextType

-- | Create a new widget
mkBoundedIntText :: IO BoundedIntText
mkBoundedIntText = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let boundedIntAttrs = defaultBoundedIntWidget "IntTextView" "BoundedIntTextModel" layout $ StyleWidget dstyle
      textAttrs = (Disabled =:: False)
                  :& (ContinuousUpdate =:: False)
                  :& (StepInt =:: Just 1)
                  :& RNil
      widgetState = WidgetState $ boundedIntAttrs <+> textAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget BoundedIntText where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["state", "value"] of
      Just (Number value) -> do
        void $ setField' widget IntValue (Sci.coefficient value)
        triggerChange widget
      _ -> pure ()
