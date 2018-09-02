{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Box.SelectionContainer.Accordion
  ( -- * The Accordion widget
    Accordion
    -- * Constructor
  , mkAccordion
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

-- | A 'Accordion' represents a Accordion widget from IPython.html.widgets.
type Accordion = IPythonWidget 'AccordionType

-- | Create a new box
mkAccordion :: IO Accordion
mkAccordion = do
  -- Default properties, with a random uuid
  wid <- U.random

  let widgetState = WidgetState $ defaultSelectionContainerWidget "AccordionView" "AccordionModel"

  stateIO <- newIORef widgetState

  let box = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen box $ toJSON widgetState

  -- Return the widget
  return box

instance IHaskellDisplay Accordion where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget Accordion where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["sync_data", "selected_index"] of
      Just (Number num) -> do
        void $ setField' widget SelectedIndex (Sci.coefficient num)
        triggerChange widget
      _ -> pure ()
