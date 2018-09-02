{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Bool.ToggleButton
  ( -- * The ToggleButton Widget
    ToggleButton
    -- * Constructor
  , mkToggleButton
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (void)
import           Data.Aeson
import           Data.IORef (newIORef)
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | A 'ToggleButton' represents a ToggleButton widget from IPython.html.widgets.
type ToggleButton = IPythonWidget 'ToggleButtonType

-- | Create a new output widget
mkToggleButton :: IO ToggleButton
mkToggleButton = do
  -- Default properties, with a random uuid
  wid <- U.random

  let boolState = defaultBoolWidget "ToggleButtonView" "ToggleButtonModel"
      toggleState = (Tooltip =:: "")
                    :& (Icon =:: "")
                    :& (ButtonStyle =:: DefaultButton)
                    :& RNil
      widgetState = WidgetState (boolState <+> toggleState)

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the image widget
  return widget

instance IHaskellDisplay ToggleButton where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget ToggleButton where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["sync_data", "value"] of
      Just (Bool value) -> do
        void $ setField' widget BoolValue value
        triggerChange widget
      _ -> pure ()
