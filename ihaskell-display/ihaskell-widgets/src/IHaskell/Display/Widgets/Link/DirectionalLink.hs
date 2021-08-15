{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.Link.DirectionalLink
  ( -- * The DirectionalLink Widget
    DirectionalLink
    -- * Constructor
  , mkDirectionalLink
    -- * Another constructor
  , jsdlink
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Data.Aeson
import           Data.IORef (newIORef)

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | An 'DirectionalLink' represents a DirectionalLink widget from IPython.html.widgets.
type DirectionalLink = IPythonWidget 'DirectionalLinkType

-- | Create a new DirectionalLink widget
mkDirectionalLink :: IO DirectionalLink
mkDirectionalLink = do
  -- Default properties, with a random uuid
  wid <- U.random

  let widgetState = WidgetState $ defaultLinkWidget "DirectionalLinkModel"

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the DirectionalLink widget
  return widget

-- | An easier constructor that links two widgets
jsdlink :: WidgetFieldPair -> WidgetFieldPair -> IO DirectionalLink
jsdlink wfp1 wfp2 = do
  dlink <- mkDirectionalLink
  _ <- setField dlink Source wfp1
  _ <- setField dlink Target wfp2
  return dlink

instance IHaskellWidget DirectionalLink where
  getCommUUID = uuid
