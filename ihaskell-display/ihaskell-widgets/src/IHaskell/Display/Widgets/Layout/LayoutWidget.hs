{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Layout.LayoutWidget
  ( -- * The Layout Widget
    Layout
    -- * Create a new Layout
  , mkLayout
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Data.Aeson
import           Data.IORef (newIORef)

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Layout.Types

-- | A 'Layout' represents a Layout from IPython.html.widgets.
type Layout = IPythonWidget 'LayoutType

-- | Create a new Layout
mkLayout :: IO Layout
mkLayout = do
  -- Default properties, with a random uuid
  wid <- U.random

  let layoutState = WidgetState defaultLayoutWidget

  stateIO <- newIORef layoutState

  let layout = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen layout $ toJSON layoutState

  -- Return the Layout widget
  return layout

instance IHaskellWidget Layout where
  getCommUUID = uuid