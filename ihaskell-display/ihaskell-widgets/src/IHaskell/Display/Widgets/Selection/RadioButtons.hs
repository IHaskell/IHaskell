{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.Selection.RadioButtons
  ( -- * The RadioButtons Widget
    RadioButtons
    -- * Constructor
  , mkRadioButtons
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (void)
import           Data.Aeson
import           Data.IORef (newIORef)
import qualified Data.Scientific as Sci

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | A 'RadioButtons' represents a RadioButtons widget from IPython.html.widgets.
type RadioButtons = IPythonWidget 'RadioButtonsType

-- | Create a new RadioButtons widget
mkRadioButtons :: IO RadioButtons
mkRadioButtons = do
  -- Default properties, with a random uuid
  wid <- U.random
  let widgetState = WidgetState $ defaultSelectionWidget "RadioButtonsView" "RadioButtonsModel"

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget RadioButtons where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["state", "index"] of
      Just (Number index) -> do
        void $ setField' widget Index (Sci.coefficient index)
        triggerSelection widget
      _ -> pure ()
