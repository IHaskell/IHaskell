{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.String.Text (
-- * The Text Widget
TextWidget, 
            -- * Constructor
            mkTextWidget) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (when)
import           Data.Aeson
import qualified Data.HashMap.Strict as Map
import           Data.IORef (newIORef)
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | A 'TextWidget' represents a Text widget from IPython.html.widgets.
type TextWidget = IPythonWidget TextType

-- | Create a new Text widget
mkTextWidget :: IO TextWidget
mkTextWidget = do
  -- Default properties, with a random uuid
  uuid <- U.random
  let strWidget = defaultStringWidget "TextView"
      txtWidget = (SubmitHandler =:: return ()) :& (ChangeHandler =:: return ()) :& RNil
      widgetState = WidgetState $ strWidget <+> txtWidget

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellDisplay TextWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget TextWidget where
  getCommUUID = uuid
  -- Two possibilities: 1. content -> event -> "submit" 2. sync_data -> value -> <new_value>
  comm tw (Object dict1) _ =
    case Map.lookup "sync_data" dict1 of
      Just (Object dict2) ->
        case Map.lookup "value" dict2 of
          Just (String val) -> setField' tw StringValue val >> triggerChange tw
          Nothing           -> return ()
      Nothing ->
        case Map.lookup "content" dict1 of
          Just (Object dict2) ->
            case Map.lookup "event" dict2 of
              Just (String event) -> when (event == "submit") $ triggerSubmit tw
              Nothing             -> return ()
          Nothing -> return ()
