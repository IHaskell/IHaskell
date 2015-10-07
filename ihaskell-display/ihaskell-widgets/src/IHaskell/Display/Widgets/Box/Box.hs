{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Box.Box (
-- * The Box widget
Box, 
     -- * Constructor
     mkBox) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Data.Aeson
import           Data.IORef (newIORef)

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types

-- | A 'Box' represents a Box widget from IPython.html.widgets.
type Box = IPythonWidget BoxType

-- | Create a new box
mkBox :: IO Box
mkBox = do
  -- Default properties, with a random uuid
  uuid <- U.random

  let widgetState = WidgetState $ defaultBoxWidget "BoxView"

  stateIO <- newIORef widgetState

  let box = IPythonWidget uuid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen box $ toJSON widgetState

  -- Return the widget
  return box

instance IHaskellDisplay Box where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget Box where
  getCommUUID = uuid
