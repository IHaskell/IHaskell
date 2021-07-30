{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Bool.CheckBox
  ( -- * The CheckBox Widget
    CheckBox
    -- * Constructor
  , mkCheckBox
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
import           IHaskell.Display.Widgets.Layout.LayoutWidget
import           IHaskell.Display.Widgets.Style.DescriptionStyle

-- | A 'CheckBox' represents a Checkbox widget from IPython.html.widgets.
type CheckBox = IPythonWidget 'CheckBoxType

-- | Create a new output widget
mkCheckBox :: IO CheckBox
mkCheckBox = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let boolAttrs = defaultBoolWidget "CheckboxView" "CheckboxModel" layout $ StyleWidget dstyle
      checkBoxAttrs = (Indent =:: True)
                      :& RNil
      widgetState = WidgetState $ boolAttrs <+> checkBoxAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the image widget
  return widget

instance IHaskellWidget CheckBox where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["state", "value"] of
      Just (Bool value) -> do
        void $ setField' widget BoolValue value
        triggerChange widget
      _ -> pure ()
