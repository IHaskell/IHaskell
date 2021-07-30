{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Style.DescriptionStyle
  ( -- * Description style
    DescriptionStyle
    -- * Create a new description style
  , mkDescriptionStyle
  ) where

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

-- | A 'DescriptionStyle' represents a Button Style from IPython.html.widgets.
type DescriptionStyle = IPythonWidget 'DescriptionStyleType

-- | Create a new button style
mkDescriptionStyle :: IO DescriptionStyle
mkDescriptionStyle = do
  wid <- U.random

  let stl = defaultStyleWidget "DescriptionStyleModel"
      but = (DescriptionWidth =:: "")
            :& RNil
      btnStlState = WidgetState (stl <+> but)

  stateIO <- newIORef btnStlState

  let style = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen style $ toJSON btnStlState

  -- Return the style widget
  return style

instance IHaskellWidget DescriptionStyle where
  getCommUUID = uuid
