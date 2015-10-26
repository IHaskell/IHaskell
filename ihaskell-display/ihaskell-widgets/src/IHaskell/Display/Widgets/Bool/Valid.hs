{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Bool.Valid (
-- * The Valid Widget
ValidWidget, 
             -- * Constructor
             mkValidWidget) where

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

-- | A 'ValidWidget' represents a Valid widget from IPython.html.widgets.
type ValidWidget = IPythonWidget ValidType

-- | Create a new output widget
mkValidWidget :: IO ValidWidget
mkValidWidget = do
  -- Default properties, with a random uuid
  uuid <- U.random

  let boolState = defaultBoolWidget "ValidView"
      validState = (ReadOutMsg =:: "") :& RNil
      widgetState = WidgetState $ boolState <+> validState

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the image widget
  return widget

instance IHaskellDisplay ValidWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget ValidWidget where
  getCommUUID = uuid
