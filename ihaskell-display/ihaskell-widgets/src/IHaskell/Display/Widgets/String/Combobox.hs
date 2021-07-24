{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.String.Combobox
  ( -- * The Combobox Widget
    ComboboxWidget
    -- * Constructor
  , mkComboboxWidget
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (when)
import           Data.Aeson
import           Data.IORef (newIORef)
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | A 'ComboboxWidget' represents a Combobox widget from IPython.html.widgets.
type ComboboxWidget = IPythonWidget 'ComboboxType

-- | Create a new Combobox widget
mkComboboxWidget :: IO ComboboxWidget
mkComboboxWidget = do
  -- Default properties, with a random uuid
  wid <- U.random
  let txtWidget = defaultTextWidget "ComboboxView" "ComboboxModel"
      boxWidget = (Options =:: [])
                  :& (EnsureOption =:: False)
                  :& RNil
      widgetState = WidgetState $ txtWidget <+> boxWidget

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget ComboboxWidget where
  getCommUUID = uuid
  -- Two possibilities: 1. content -> event -> "submit" 2. sync_data -> value -> <new_value>
  comm tw val _ = do
    case nestedObjectLookup val ["state", "value"] of
      Just (String value) -> setField' tw StringValue value >> triggerChange tw
      _                 -> pure ()
    case nestedObjectLookup val ["content", "event"] of
      Just (String event) -> when (event == "submit") $ triggerSubmit tw
      _                   -> pure ()