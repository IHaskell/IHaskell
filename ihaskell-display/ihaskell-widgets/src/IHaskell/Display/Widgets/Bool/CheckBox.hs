{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Bool.CheckBox (
-- * The CheckBox Widget
CheckBox, 
          -- * Constructor
          mkCheckBox) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Data.Aeson
import           Data.HashMap.Strict as HM
import           Data.IORef (newIORef)
import           Data.Text (Text)

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | A 'CheckBox' represents a Checkbox widget from IPython.html.widgets.
type CheckBox = IPythonWidget CheckBoxType

-- | Create a new output widget
mkCheckBox :: IO CheckBox
mkCheckBox = do
  -- Default properties, with a random uuid
  uuid <- U.random

  let widgetState = WidgetState $ defaultBoolWidget "CheckboxView"

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the image widget
  return widget

instance IHaskellDisplay CheckBox where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget CheckBox where
  getCommUUID = uuid
  comm widget (Object dict1) _ = do
    let key1 = "sync_data" :: Text
        key2 = "value" :: Text
        Just (Object dict2) = HM.lookup key1 dict1
        Just (Bool value) = HM.lookup key2 dict2
    setField' widget BoolValue value
    triggerChange widget
