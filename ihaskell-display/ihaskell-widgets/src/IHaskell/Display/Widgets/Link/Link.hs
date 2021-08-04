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
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Data.Aeson
import           Data.IORef (newIORef)
import           Data.Monoid (mempty)
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import           IHaskell.Display.Widgets.Layout.LayoutWidget

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

instance IHaskellWidget Link where
  getCommUUID = uuid
