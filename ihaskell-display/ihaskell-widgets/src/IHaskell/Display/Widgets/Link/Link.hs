{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.Link.Link
  ( -- * The Link Widget
    Link
    -- * Constructor
  , mkLink
    -- * Easier constructor
  , jslink
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

-- | An 'Link' represents a Link widget from IPython.html.widgets.
type Link = IPythonWidget 'LinkType

-- | Create a new link widget
mkLink :: IO Link
mkLink = do
  -- Default properties, with a random uuid
  wid <- U.random

  let widgetState = WidgetState $ defaultLinkWidget "LinkModel"

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the link widget
  return widget

-- | An easier constructor that links two widgets
jslink :: WidgetFieldPair -> WidgetFieldPair -> IO Link
jslink wfp1 wfp2 = do
  link <- mkLink
  _ <- setField link Source wfp1
  _ <- setField link Target wfp2
  return link

instance IHaskellWidget Link where
  getCommUUID = uuid
