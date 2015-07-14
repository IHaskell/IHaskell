{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Box.FlexBox (
    -- * The FlexBox widget
    FlexBox,
    -- * Constructor
    mkFlexBox,
    ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (when, join)
import           Data.Aeson
import           Data.HashMap.Strict as HM
import           Data.IORef (newIORef)
import           Data.Text (Text)
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | A 'FlexBox' represents a FlexBox widget from IPython.html.widgets.
type FlexBox = IPythonWidget FlexBoxType

-- | Create a new box
mkFlexBox :: IO FlexBox
mkFlexBox = do
  -- Default properties, with a random uuid
  uuid <- U.random

  let boxAttrs = defaultBoxWidget "FlexBoxView"
      flxAttrs = (SOrientation =:: HorizontalOrientation)
              :& (SFlex =:: 0)
              :& (SPack =:: StartLocation)
              :& (SAlign =:: StartLocation)
              :& RNil
      widgetState = WidgetState $ boxAttrs <+> flxAttrs

  stateIO <- newIORef widgetState

  let box = IPythonWidget uuid stateIO
      initData = object ["model_name" .= str "WidgetModel", "widget_class" .= str "IPython.FlexBox"]

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen box initData $ toJSON widgetState

  -- Return the widget
  return box

instance IHaskellDisplay FlexBox where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget FlexBox where
  getCommUUID = uuid
