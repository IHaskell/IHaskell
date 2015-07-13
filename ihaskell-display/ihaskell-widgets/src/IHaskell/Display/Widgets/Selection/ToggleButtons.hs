{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Selection.ToggleButtons (
-- * The ToggleButtons Widget
ToggleButtons,
               -- * Constructor
               mkToggleButtons) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (when, join, void)
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

-- | A 'ToggleButtons' represents a ToggleButtons widget from IPython.html.widgets.
type ToggleButtons = IPythonWidget ToggleButtonsType

-- | Create a new ToggleButtons widget
mkToggleButtons :: IO ToggleButtons
mkToggleButtons = do
  -- Default properties, with a random uuid
  uuid <- U.random
  let selectionAttrs = defaultSelectionWidget "ToggleButtonsView"
      toggleButtonsAttrs = (STooltips =:: [])
                           :& (SIcons =:: [])
                           :& (SButtonStyle =:: DefaultButton)
                           :& RNil
      widgetState = WidgetState $ selectionAttrs <+> toggleButtonsAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO
      initData = object
                   [ "model_name" .= str "WidgetModel"
                   , "widget_class" .= str "IPython.ToggleButtons"
                   ]

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget initData $ toJSON widgetState

  -- Return the widget
  return widget

-- | Artificially trigger a selection
triggerSelection :: ToggleButtons -> IO ()
triggerSelection widget = join $ getField widget SSelectionHandler

instance IHaskellDisplay ToggleButtons where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget ToggleButtons where
  getCommUUID = uuid
  comm widget (Object dict1) _ = do
    let key1 = "sync_data" :: Text
        key2 = "selected_label" :: Text
        Just (Object dict2) = HM.lookup key1 dict1
        Just (String label) = HM.lookup key2 dict2
    opts <- getField widget SOptions
    case opts of
      OptionLabels _ -> void $ do
        setField' widget SSelectedLabel label
        setField' widget SSelectedValue label
      OptionDict ps ->
        case lookup label ps of
          Nothing -> return ()
          Just value -> void $ do
            setField' widget SSelectedLabel label
            setField' widget SSelectedValue value
    triggerSelection widget
