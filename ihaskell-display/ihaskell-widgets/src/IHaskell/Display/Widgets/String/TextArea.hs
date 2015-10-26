{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.String.TextArea (
-- * The TextArea Widget
TextArea, 
          -- * Constructor
          mkTextArea) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

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

-- | A 'TextArea' represents a Textarea widget from IPython.html.widgets.
type TextArea = IPythonWidget TextAreaType

-- | Create a new TextArea widget
mkTextArea :: IO TextArea
mkTextArea = do
  -- Default properties, with a random uuid
  uuid <- U.random
  let strAttrs = defaultStringWidget "TextareaView"
      wgtAttrs = (ChangeHandler =:: return ()) :& RNil
      widgetState = WidgetState $ strAttrs <+> wgtAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellDisplay TextArea where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget TextArea where
  getCommUUID = uuid
  comm widget (Object dict1) _ = do
    let key1 = "sync_data" :: Text
        key2 = "value" :: Text
        Just (Object dict2) = HM.lookup key1 dict1
        Just (String value) = HM.lookup key2 dict2
    setField' widget StringValue value
    triggerChange widget
