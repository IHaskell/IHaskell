{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.Selection.Select
  ( -- * The Select Widget
    Select
    -- * Constructor
  , mkSelect
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (void)
import           Data.Aeson
import           Data.IORef (newIORef)
import qualified Data.Scientific as Sci
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import           IHaskell.Display.Widgets.Layout.LayoutWidget
import           IHaskell.Display.Widgets.Style.DescriptionStyle

-- | A 'Select' represents a Select widget from IPython.html.widgets.
type Select = IPythonWidget 'SelectType

-- | Create a new Select widget
mkSelect :: IO Select
mkSelect = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout
  dstyle <- mkDescriptionStyle

  let selectionAttrs = defaultSelectionWidget "SelectView" "SelectModel" layout $ StyleWidget dstyle
      selectAttrs = (Rows =:: Just 5)
                    :& RNil
      widgetState = WidgetState $ selectionAttrs <+> selectAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget Select where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["state", "index"] of
      Just (Number index) -> do
        void $ setField' widget OptionalIndex (Just $ Sci.coefficient index)
        triggerSelection widget
      _ -> pure ()
