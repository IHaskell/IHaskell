{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.DatePicker
  ( -- * The DatePicker Widget
    DatePicker
    -- * Create a new DatePicker
  , mkDatePicker
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

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

-- | A 'DatePicker' represents a DatePicker from IPython.html.widgets.
type DatePicker = IPythonWidget 'DatePickerType

-- | Create a new DatePicker
mkDatePicker :: IO DatePicker
mkDatePicker = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let ddw = defaultDescriptionWidget "DatePickerView" "DatePickerModel" layout $ StyleWidget dstyle
      date = (DateValue =:: defaultDate)
              :& (Disabled =:: False)
              :& (ChangeHandler =:: return ())
              :& RNil
      datePickerState = WidgetState (ddw <+> date)

  stateIO <- newIORef datePickerState

  let datePicker = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen datePicker $ toJSON datePickerState

  -- Return the DatePicker widget
  return datePicker

instance IHaskellWidget DatePicker where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["state", "value"] of
      Just o -> case fromJSON o of
        Success date -> setField' widget DateValue date >> triggerChange widget
        _ -> pure ()
      _ -> pure ()