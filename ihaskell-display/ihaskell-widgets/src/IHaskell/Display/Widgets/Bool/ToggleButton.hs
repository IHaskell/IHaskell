{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Bool.ToggleButton (
-- * The ToggleButton Widget
ToggleButton, 
              -- * Constructor
              mkToggleButton) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Data.Aeson
import           Data.HashMap.Strict as HM
import           Data.IORef (newIORef)
import           Data.Text (Text)
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | A 'ToggleButton' represents a ToggleButton widget from IPython.html.widgets.
type ToggleButton = IPythonWidget ToggleButtonType

-- | Create a new output widget
mkToggleButton :: IO ToggleButton
mkToggleButton = do
  -- Default properties, with a random uuid
  uuid <- U.random

  let boolState = defaultBoolWidget "ToggleButtonView"
      toggleState = (Tooltip =:: "")
                    :& (Icon =:: "")
                    :& (ButtonStyle =:: DefaultButton)
                    :& RNil
      widgetState = WidgetState (boolState <+> toggleState)

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO

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
  comm widget (Object dict1) _ = do
    let key1 = "sync_data" :: Text
        key2 = "value" :: Text
        Just (Object dict2) = HM.lookup key1 dict1
        Just (Bool value) = HM.lookup key2 dict2
    setField' widget BoolValue value
    triggerChange widget
