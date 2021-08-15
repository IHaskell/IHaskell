{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module IHaskell.Display.Widgets.Controller.Controller
  ( -- * The Controller Widget
    Controller
    -- * Constructor
  , mkController
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (void)

import           Data.Aeson
import           Data.Aeson.Types (parse)
import           Data.IORef (newIORef)
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import           IHaskell.Display.Widgets.Layout.LayoutWidget

-- | 'Controller' represents an Controller widget from IPython.html.widgets.
type Controller = IPythonWidget 'ControllerType

-- | Create a new widget
mkController :: IO Controller
mkController = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout

  let domAttrs = defaultCoreWidget <+> defaultDOMWidget "ControllerView" "ControllerModel" layout
      ctrlAttrs = (Index =:+ 0)
                  :& (Name =:! "")
                  :& (Mapping =:! "")
                  :& (Connected =:! False)
                  :& (Timestamp =:! 0.0)
                  :& (Buttons =:! [])
                  :& (Axes =:! [])
                  :& (ChangeHandler =:: pure ())
                  :& RNil
      widgetState = WidgetState $ domAttrs <+> ctrlAttrs

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the widget
  return widget

instance IHaskellWidget Controller where
  getCommUUID = uuid
  comm widget val _ =
    case nestedObjectLookup val ["state"] of
        Just (Object o) -> do
            parseAndSet Name "name"
            parseAndSet Mapping "mapping"
            parseAndSet Connected "connected"
            parseAndSet Timestamp "timestamp"
            triggerChange widget
            where parseAndSet f s = case parse (.: s) o of
                    Success x -> void $ setField' widget f x
                    _ -> pure ()
        _ -> pure ()
