{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

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
#if MIN_VERSION_aeson(2,0,0)
#else
import           Data.Text (Text)
#endif
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import           IHaskell.Display.Widgets.Layout.LayoutWidget

-- | 'Controller' represents an Controller widget from IPython.html.widgets.
type Controller = IPythonWidget ControllerType

-- | Create a new widget
mkController :: IO Controller
mkController = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout

  let domAttrs = defaultCoreWidget <+> defaultDOMWidget "ControllerView" "ControllerModel" layout
      ctrlAttrs = (F @Index =:+ 0)
                  :& (F @Name =:! "")
                  :& (F @Mapping =:! "")
                  :& (F @Connected =:! False)
                  :& (F @Timestamp =:! 0.0)
                  :& (F @Buttons =:! [])
                  :& (F @Axes =:! [])
                  :& (F @ChangeHandler =:: pure ())
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
            parseAndSet @Name "name"
            parseAndSet @Mapping "mapping"
            parseAndSet @Connected "connected"
            parseAndSet @Timestamp "timestamp"
            triggerChange widget
#if MIN_VERSION_aeson(2,0,0)
            where parseAndSet :: forall f. (RElemOf f (WidgetFields ControllerType),
                                      IHaskellWidget Controller,
                                      ToKey f, FromJSON (FieldType f)) => Key -> IO ()
#else
            where parseAndSet :: forall f. (RElemOf f (WidgetFields ControllerType),
                                      IHaskellWidget Controller,
                                      ToKey f, FromJSON (FieldType f)) => Text -> IO ()
#endif
                  parseAndSet s = case parse (.: s) o of
                    Success x -> void $ setField' @f widget x
                    _ -> pure ()
        _ -> pure ()
