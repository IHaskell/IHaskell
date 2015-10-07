{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Selection.Dropdown (
-- * The Dropdown Widget
Dropdown, 
          -- * Constructor
          mkDropdown) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (void)
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.IORef (newIORef)
import           Data.Text (Text)
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | A 'Dropdown' represents a Dropdown widget from IPython.html.widgets.
type Dropdown = IPythonWidget DropdownType

-- | Create a new Dropdown widget
mkDropdown :: IO Dropdown
mkDropdown = do
  -- Default properties, with a random uuid
  uuid <- U.random
  let selectionAttrs = defaultSelectionWidget "DropdownView"
      dropdownAttrs = (ButtonStyle =:: DefaultButton) :& RNil
      widgetState = WidgetState $ selectionAttrs <+> dropdownAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellDisplay Dropdown where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget Dropdown where
  getCommUUID = uuid
  comm widget (Object dict1) _ = do
    let key1 = "sync_data" :: Text
        key2 = "selected_label" :: Text
        Just (Object dict2) = HM.lookup key1 dict1
        Just (String label) = HM.lookup key2 dict2
    opts <- getField widget Options
    case opts of
      OptionLabels _ -> void $ do
        setField' widget SelectedLabel label
        setField' widget SelectedValue label
      OptionDict ps ->
        case lookup label ps of
          Nothing -> return ()
          Just value -> void $ do
            setField' widget SelectedLabel label
            setField' widget SelectedValue value
    triggerSelection widget
