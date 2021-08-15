{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.Selection.ToggleButtons
  ( -- * The ToggleButtons Widget
    ToggleButtons
    -- * Constructor
  , mkToggleButtons
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

-- | A 'ToggleButtons' represents a ToggleButtons widget from IPython.html.widgets.
type ToggleButtons = IPythonWidget 'ToggleButtonsType

-- | Create a new ToggleButtons widget
mkToggleButtons :: IO ToggleButtons
mkToggleButtons = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let selectionAttrs = defaultSelectionWidget "ToggleButtonsView" "ToggleButtonsModel" layout $ StyleWidget dstyle
      toggleButtonsAttrs = (Tooltips =:: [])
                           :& (Icons =:: [])
                           :& (ButtonStyle =:: DefaultButton)
                           :& RNil
      widgetState = WidgetState $ selectionAttrs <+> toggleButtonsAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget ToggleButtons where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["state", "index"] of
      Just (Number index) -> do
        void $ setField' widget OptionalIndex (Just $ Sci.coefficient index)
        triggerSelection widget
      _ -> pure ()
