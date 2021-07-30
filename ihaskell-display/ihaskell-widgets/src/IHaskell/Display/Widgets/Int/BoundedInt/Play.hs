{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Int.BoundedInt.Play
  ( -- * The Play Widget
    Play
    -- * Constructor
  , mkPlay
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (void)
import           Data.Aeson
import           Data.IORef (newIORef)
import qualified Data.Scientific as Sci
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display (IHaskellWidget(..))
import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import           IHaskell.Display.Widgets.Layout.LayoutWidget
import           IHaskell.Display.Widgets.Style.DescriptionStyle

-- | 'Play' represents an Play widget from IPython.html.widgets.
type Play = IPythonWidget 'PlayType

-- | Create a new widget
mkPlay :: IO Play
mkPlay = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let boundedIntAttrs = defaultBoundedIntWidget "PlayView" "PlayModel" layout $ StyleWidget dstyle
      playAttrs = (Playing =:: True)
                  :& (Repeat =:: True)
                  :& (Interval =:: 100)
                  :& (StepInt =:: Just 1)
                  :& (Disabled =:: False)
                  :& (ShowRepeat =:: True)
                  :& RNil
      widgetState = WidgetState $ boundedIntAttrs <+> playAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget Play where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["state", "value"] of
      Just (Number value) -> do
        void $ setField' widget IntValue (Sci.coefficient value)
        triggerChange widget
      _ -> pure ()
