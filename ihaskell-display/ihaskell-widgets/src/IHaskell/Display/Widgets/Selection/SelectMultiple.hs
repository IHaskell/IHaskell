{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Selection.SelectMultiple (
-- * The SelectMultiple Widget
SelectMultiple, 
                -- * Constructor
                mkSelectMultiple) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (void)
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.IORef (newIORef)
import           Data.Text (Text)
import qualified Data.Vector as V

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | A 'SelectMultiple' represents a SelectMultiple widget from IPython.html.widgets.
type SelectMultiple = IPythonWidget SelectMultipleType

-- | Create a new SelectMultiple widget
mkSelectMultiple :: IO SelectMultiple
mkSelectMultiple = do
  -- Default properties, with a random uuid
  uuid <- U.random
  let widgetState = WidgetState $ defaultMultipleSelectionWidget "SelectMultipleView"

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellDisplay SelectMultiple where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget SelectMultiple where
  getCommUUID = uuid
  comm widget (Object dict1) _ = do
    let key1 = "sync_data" :: Text
        key2 = "selected_labels" :: Text
        Just (Object dict2) = HM.lookup key1 dict1
        Just (Array labels) = HM.lookup key2 dict2
        labelList = map (\(String x) -> x) $ V.toList labels
    opts <- getField widget Options
    case opts of
      OptionLabels _ -> void $ do
        setField' widget SelectedLabels labelList
        setField' widget SelectedValues labelList
      OptionDict ps ->
        case sequence $ map (`lookup` ps) labelList of
          Nothing -> return ()
          Just valueList -> void $ do
            setField' widget SelectedLabels labelList
            setField' widget SelectedValues valueList
    triggerSelection widget
