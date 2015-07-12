{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Selection.Select (
-- * The Select Widget
Select,
              -- * Constructor
              mkSelect) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (when, join)
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

-- | A 'Select' represents a Select widget from IPython.html.widgets.
type Select = IPythonWidget SelectType

-- | Create a new Select widget
mkSelect :: IO Select
mkSelect = do
  -- Default properties, with a random uuid
  uuid <- U.random
  let widgetState = WidgetState $ defaultSelectionWidget "SelectView"

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO
      initData = object ["model_name" .= str "WidgetModel", "widget_class" .= str "IPython.Select"]

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget initData $ toJSON widgetState

  -- Return the widget
  return widget

-- | Artificially trigger a selection
triggerSelection :: Select -> IO ()
triggerSelection widget = join $ getField widget SSelectionHandler

instance IHaskellDisplay Select where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget Select where
  getCommUUID = uuid
  comm widget (Object dict1) _ = do
    let key1 = "sync_data" :: Text
        key2 = "selected_label" :: Text
        Just (Object dict2) = HM.lookup key1 dict1
        Just (String label) = HM.lookup key2 dict2
    opts <- getField widget SOptions
    case opts of
      OptionLabels _ -> do
        setField' widget SSelectedLabel label
        setField' widget SSelectedValue label
      OptionDict ps ->
        case lookup label ps of
          Nothing -> return ()
          Just value -> do
            setField' widget SSelectedLabel label
            setField' widget SSelectedValue value
    triggerSelection widget
